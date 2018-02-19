(use-modules (ice-9 peg string-peg))
(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))


#!
We should be able to parse
(3625/10625 * 24999.76)
(ceil(2626.77+2999.0)
rc[-1]-rc[-2]
if(rc[-2]<0, -rc[-2], "")

And do some clever compiling like so:
(define foo (compile '(lambda (x) (+ x 13))))
(define h (make-hash-table))
(hash-set! h 1 2)
(h-ref h 1)
!#





(define-peg-pattern dq body "\"") ; double-quote. Easier to define it separately
(define-peg-string-patterns
  "string-pattern <-- dq string-rest dq
  string-rest <- (! dq .)*")


(define-peg-string-patterns
  "expr <- function/string-pattern/expr-r/num
  function <-- function-name '(' expr ')'
  function-name <-- [a-z]+
  expr-r <-- num op num
  op <-- '+' / '-' / '*' / '/'
  W <-- [ \t\n]
  num <-- [0-9]+ ('.' [0-9]+)?
  ")

(define (parse pat str)
  (peg:tree (match-pattern pat str)))

(define first car)
(define rest cdr)
(define empty? null?)
(define null '())

(define (flatten l)
  (cond 
    ((empty? l)      null)
    ((not (list? l)) (list l))
    (else            (append (flatten (first l)) (flatten (rest l))))))

(define unparse flatten)


(define (show pat str)
  (define tree (parse pat str))
  (format #t "\nParsing: ~s\n" str)
  (pretty-print tree)
  (format #t "\nUnparsing:\n")
  (pretty-print (unparse tree))
  #t)

#!
(show formula "hello(12)")
(show formula "42.42")
(show formula "hello(\"world\")")
(show formula "32+34.45")
(show string-pattern "\"this is a String\"")
!#

;;; --- oh hell, it might just be easier to hand-roll everyhting

(format #t "\n\nNow just doing it my own way\n")

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
		((_ s n)
		 (set! s (string-append s n)))))

(define-syntax string-drop!
  (syntax-rules ()
		((_ s n)
		 (set! s (string-drop s n)))))

(define (get-string s)
  (define result DQ)
  (string-drop! s 1)
  (let loop ()
    (unless (or (string-null? s) (dq0? s))
      ;(write-ln s)
      (string-append! result (string-take s 1))
      (string-drop! s 1)
      (loop)))
  ;(format #t "string is ~s\n" result)
  (string-append result DQ))

(define (alpha? s) 
  (if (string-null? s)
    #f
    (char-alphabetic? (car (string->list s 0 1)))))

(define (parse-cell-or-func s)
  ;; TODO handle cell references
  (define result "")
  (let loop ()
    (when (alpha? s)
      (set! result (string-append result(string-take s 1)))
      (set! s (string-drop s 1))
      (loop)))
    result)



(define (inner-bfp str)
  (define c0 (string-take str 1))
  (define (c0? c) (equal? c0 c))
  (cond
    ((c0? "\"") (get-string str))
    ((alpha? c0) (parse-cell-or-func str))
    (#t 'parse-error)))

;;; big flipping parser
(define (bfp str)
  (define result '())
  (define match "")
  (let loop ()
    (set! str (string-trim str char-set:whitespace))
    (unless (string-null? str)
      (set! match (inner-bfp str))
      (if (eq? match 'parse-error)
	(set! result 'parse-error)
	(begin
	  (set! result (cons match result))
	  (set! str (string-drop str (string-length match)))
	  (loop)))))

  (if (list? result)
    (reverse result)
    result))


(define (parse-test str) 
  (format #t "\nParsing ~s\n" str)
  (format #t "~s\n" (bfp str))
  (format #t "Finished parsing\n"))

(parse-test " \"hellow world\" \"another string\"")
(parse-test " hellow ")

