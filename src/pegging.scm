;;;; experiments with the peg module


(use-modules (ice-9 peg string-peg))
(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))


(define-peg-pattern DQ body "\"")
;(define-peg-pattern NDQ body "!\"")
(define-peg-string-patterns "astring <-- DQ (! DQ .)* DQ")


(define-peg-string-patterns  
  "expr <- W sum W
  sum <- (product W ('+'/'-') W sum) / product
  product <- W (value W ('*'/'/') W product) / value
  value <- W number / astring / '(' expr ')' / call
  call <-- name '(' arglist ')'
  arglist <-- (expr ',' arglist) / expr 
  name <-- [a-zA-Z]+
  number <-- [0-9]+
  W < [ \t\n]*")

(peg:tree (match-pattern num "23.45"))
(peg:tree (match-pattern formula "asd23.3"))
(peg:tree (match-pattern formula "23+24"))
;(write (peg:tree (match-pattern astring "\"hello\"")))

(define (pma str) (peg:tree (match-pattern DQ str)))
(define (pma1 str) (peg:tree (match-pattern astring str)))
(define (pma2 str) (pretty-print (peg:tree (match-pattern expr str))))

(pma2 "12+13")
