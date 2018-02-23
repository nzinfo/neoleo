;;(use-modules (ice-9 peg string-peg))
;;(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))
(use-modules (oop goops))
(use-modules (ice-9 i18n))

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

(define (accept-lexeme pb)
  (slot-set! pb 'accepted-lexemes (append (slot-ref pb 'accepted-lexemes) (list (slot-ref pb 'lexeme))))
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
  (accept-lexeme pb) 
  pb)

(define-method (parse-string (str <string>))
	       (parse-string (make-pb str)))

(define (more? pb) (not (finished? pb)))

(define (try-numeric pb)
  (define-values (num nread)  (locale-string->inexact (pb-unprocessed pb)))
  (when (positive? nread)
    (pb-lexeme! pb num)
    (accept-lexeme pb)
    (eat-chars pb nread))
  (positive? nread))

(define (parse-word-or-ref pb)
  (snarfing pb char-alphabetic?) ; TODO handle case of cell ref
  (accept-lexeme pb))

(define (accept-char pb)
  (take-char pb)
  (accept-lexeme pb))

(define (get-lexeme pb)
  (define c0 (peek pb))
  (cond
    ((memq c0 (list #\+ #\-)) (accept-char pb))
    ((try-numeric pb))
    ((eq? c0 #\") (parse-string pb))
    ((char-alphabetic? c0) (parse-word-or-ref pb))
    (#t (accept-char pb)))
  #t)

;;; big flipping parser
(define (bfp str)
  (define pb (make-pb str))
  (while (more? (eat-white pb))
	 (get-lexeme pb))
  pb)




(define (parse-test str) 
  (define pb (bfp str))
  ;;(write (slot-ref pb 'lexeme))
  (format #t "\nParsing ~s\n" str)
  (format #t "Result: ~s\n" (describe-pb pb))
  (format #t "Finished parsing\n"))

(define (run)
  (parse-test " \"hellow world\" \"another string\"")
  (parse-test " hellow ")
  (parse-test "2345 hello")
  (parse-test "-23+24")
  #t)




(define (expr tokens) 'defined-below)


(define (bin-op op token rest)
       (list op token  (expr rest)))

(define (expr tokens)
  (define curr-token (nex-car tokens))
  (define next-token (nex-cadr tokens))
  (cond 
    ((number? curr-token)
     (if (equal? "+" next-token)
       (bin-op "+" curr-token (nex-cddr tokens))
       curr-token))

    ;;((string? current-token) 


    (#t "#PARSE")))



(define (guile-parse str)
  (define tokens (pb-lexemes (bfp str)))
  (expr tokens))

	


(define (run1)
  (guile-parse "23+24"))
