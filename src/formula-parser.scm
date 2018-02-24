;;(use-modules (ice-9 peg string-peg))
;;(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))
(use-modules (oop goops))
(use-modules (ice-9 i18n))
(use-modules (ice-9 match))

(define LRB #\()
(define RRB #\))

;; don't throw toys out of pram if we can't do it
(define-syntax no-except
  (syntax-rules ()
		((_ proc els)
		 (catch #t
			(lambda () (proc els))
			(lambda (key . params) 'oops)))))

(define (nex-car els) (no-except car els))
(define (nex-cadr els) (no-except cadr els))
(define (nex-caddr els) (no-except caddr els))
(define (nex-cddr els) (no-except cddr els))


#! operator precedence in accordance with java:
https://introcs.cs.princeton.edu/java/11precedence/

12 * / %
11 + -
9 < <= > >= <>
!#

#!



We should be able to parse
(3625/10625 * 24999.76)
(ceil(2626.77+2999.0)
rc[-1]-rc[-2]
if(rc[-2]<0, -rc[-2], "")

And do some clever compiling like so:
http://lists.gnu.org/archive/html/guile-user/2018-02/msg00006.html
(define foo (compile '(lambda (x) (+ x 13))))
(define h (make-hash-table))
(hash-set! h 1 2)
(h-ref h 1)
!#

;;; --- oh hell, it might just be easier to hand-roll everyhting

(define-class <parser-buffer> (<object>)
	      (accepted-lexemes #:init-value '() #:getter pb-lexemes) ; the completed lexemes
	      (lexeme #:init-value "" #:setter pb-lexeme!) ; the lexeme currently being built
	      (unprocessed #:init-keyword #:unprocessed #:getter pb-unprocessed))

(define-method (eat-white (pb <parser-buffer>))
	       (slot-set!  pb 'unprocessed (string-trim (pb-unprocessed pb) char-set:whitespace))
	       pb)

(define-generic peek)

(define-method (peek (pb <parser-buffer>))
	       (define s (slot-ref pb 'unprocessed))
	       (if (string-null? s)
		 'eof
		 (string-ref s 0)))

;(define-method (peek? (pb <parser-buffer>) (c <char>))
;	       (equal? c 

(define (accept-lexeme pb type)
  (slot-set! pb 'accepted-lexemes (append (slot-ref pb 'accepted-lexemes) (list (list type (slot-ref pb 'lexeme)))))
  (slot-set! pb 'lexeme "")
  pb)
  
(define-method (finished? (pb <parser-buffer>))
	       (string-null? (slot-ref pb 'unprocessed)))

(define (eat-chars pb n)
	       (slot-set! pb 'unprocessed (string-drop (slot-ref pb 'unprocessed) n)))

(define (eat-char pb) (eat-chars pb 1))

(define (make-pb str)
    (make <parser-buffer> #:unprocessed str))


(define (describe-pb pb)
  (define (f el) (list el (slot-ref pb el)))
  (write (map f '(accepted-lexemes lexeme unprocessed)))
  (newline))

(define (char0 s)
  (if (string-null? s)
    ""
    (string-take s 1)))

(define DQ "\"")

(define (dq0? s)
  (equal? (char0 s) "\""))

(define (write-ln s) 
  (write s)
  (newline))
	
(define-syntax string-append!
  (syntax-rules ()
		((_ s arg1 ... )
		 (set! s (string-append s arg1 ...)))))

(define-syntax string-drop!
  (syntax-rules ()
		((_ s n)
		 (set! s (string-drop s n)))))

;;; remove the first char from string `remainder' and put it to end of `lexeme'
(define-syntax take-char!
  (syntax-rules ()
		((_ lexeme remainder)
		 (begin
		   (string-append! lexeme (string-take remainder 1))
		   (string-drop! remainder 1)))))


(define (char-type? pred s)
  (if (string-null? s)
    #f
    (pred (car (string->list s 0 1)))))


(define (alpha? s) (char-type? char-alphabetic? s))

(define (numeric? s) (char-type? char-numeric? s))



(define (take-char pb)
  (define s (slot-ref pb 'unprocessed))
  (slot-set! pb 'lexeme (string-append (slot-ref pb 'lexeme) (string (peek pb))))
  (eat-char pb))



(define (snarfing pb pred)
    (define c0 (peek pb))
    (when (char? c0)
      (when (pred c0)
	(take-char pb)
	(snarfing pb pred))))

(define (not-dq? c) (not (eq? c #\")))

(define-generic parse-string)

(define-method (parse-string (pb <parser-buffer>))
  (take-char pb) ; the double-quote
  (snarfing pb not-dq?)
  (take-char pb) ; the end-quote
  (accept-lexeme pb 'double-quotes) 
  pb)

(define-method (parse-string (str <string>))
	       (parse-string (make-pb str)))

(define (more? pb) (not (finished? pb)))

(define (parse-<> pb) ; parse for <. <=, >, >=, <>
  (take-char pb)
  (when (memq (peek pb) '(#\= #\>))
    (take-char pb))
  (accept-lexeme pb 'bin-op-9))

(define (try-numeric pb)
  (define-values (num nread)  (locale-string->inexact (pb-unprocessed pb)))
  (when (positive? nread)
    (pb-lexeme! pb num)
    (accept-lexeme pb 'number)
    (eat-chars pb nread))
  (positive? nread))

(define (parse-word-or-ref pb)
  (snarfing pb char-alphabetic?) ; TODO handle case of cell ref
  (accept-lexeme pb 'chars))

(define (accept-char-type pb type)
  (take-char pb)
  (accept-lexeme pb type))

(define (accept-char pb) (accept-char-type pb 'char))

(define (get-lexeme pb)
  (define c0 (peek pb))
  (cond
    ((memq c0 (list #\+ #\-)) (accept-char-type pb 'bin-op-11))
    ((memq c0 (list #\* #\/)) (accept-char-type pb 'bin-op-12))
    ((memq c0 (list #\< #\>)) (parse-<> pb))
    ((try-numeric pb))
    ((eq? c0 #\") (parse-string pb))
    ((char-alphabetic? c0) (parse-word-or-ref pb))
    (#t (accept-char pb)))
  #t)

(define (tokenise str)
  (define pb (make-pb str))
  (while (more? (eat-white pb))
	 (get-lexeme pb))
  (pb-lexemes pb))




(define (parse-test str) 
  (format #t "\nParsing ~s\n" str)
  (format #t "Result: ~s\n" (tokenise str))
  (format #t "Finished parsing\n"))

(define (run1)
  (parse-test " \"hellow world\" \"another string\"")
  (parse-test " hellow ")
  (parse-test "2345 hello")
  (parse-test "-23+24")
  #t)




(define (expr-e tokens) 'defined-below)


;(define (bin-op op token rest)
;       (list op token  (expr rest)))

(define (expr-p tokens)
  (match tokens
	 ((('number n)) (list 'expr-p n))
	 ((('char "(") ('expr-e e) ('char ")")) (list 'expr-e e))))

(define (expr-r tokens)
  (match tokens
	 (((t1 v1)) (list expr-r v1))
	 (((t1 v1) ('bin-op-11 op) . rest) (list op v1 

(define (expr-e tokens)
  (match tokens
	 (((t1 v1)) 
	 (((t1 v1) ('bin-op-9 op) . rest) (list op v1 (expr-r rest)))))


;(define (expr-e-XXX tokens)
;  (define curr-token (nex-car tokens))
;  (define next-token (nex-cadr tokens))
;  (cond 
;    ((number? curr-token)
;     (if (equal? "+" next-token)
;       (bin-op "+" curr-token (nex-cddr tokens))
;       curr-token))
;
;    ;;((string? current-token) 
;
;
;    (#t "#PARSE")))




(define (guile-parse str)
  (expr-e (tokenise str))) 

	


(define (run2)
  (guile-parse "23+24"))
