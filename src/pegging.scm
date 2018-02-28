;;;; experiments with the peg module


(use-modules (ice-9 peg string-peg))
(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))
(use-modules (oop goops))


(define-peg-pattern DQ body "\"")
;(define-peg-pattern NDQ body "!\"")
(define-peg-string-patterns "astring <-- DQ (! DQ .)* DQ")


(define-peg-string-patterns  
  "expr <- W unary W
  unary <- ('+'/'-')? sum
  sum <- (product W ('+'/'-') W sum) / product
  product <- W (value W ('*'/'/') W product) / value
  value <- W number / astring / bra / call
  bra <-- LRB expr RRB
  call <-- name '(' arglist ')'
  arglist <-- (expr ',' arglist) / expr 
  name <-- [a-zA-Z]+
  number <- float/int
  float <-- int '.' int
  int <-- [0-9]+
  LRB < '('
  RRB < ')'
  W < [ \t\n]*")

(peg:tree (match-pattern number "23.45"))
;(peg:tree (match-pattern formula "asd23.3"))
;(peg:tree (match-pattern formula "23+24"))
;(write (peg:tree (match-pattern astring "\"hello\"")))


(define (make-tree-pat pat str)  (peg:tree (match-pattern pat str)))
(define (make-tree str) (make-tree-pat expr str))

(define (pma str) (make-tree-pat DQ str))
(define (pma1 str) (make-tree-pat astring str))
(define (pma2 str) (pretty-print (make-tree str)))

(pma2 "12+13")

(define (transform-tree tree)
  (match tree
	 (("+" ('int n)) (string->number n))
	 (("-" ('int n)) (string->number n))
	 (('int n) (string->number n))))

(define (parse str)
  (transform-tree (make-tree str)))
