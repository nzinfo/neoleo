;;;; experiments with the peg module


(use-modules (ice-9 peg string-peg))
(use-modules (ice-9 peg using-parsers))

(define-peg-pattern DQ body "\"")
;(define-peg-pattern NDQ body "!\"")
(define-peg-pattern astring body "(DQ .*)")


(define-peg-string-patterns  
  "formula <-- word/flt/(num op num)/num/astring
  word <--[a-zA-Z]+
  num <-- flt/digits 
  flt <-- (digits '.' digits)
  op <-- '<='/'<'/'+'/'-'/'('/')'
  digits <-- [0-9]+ ")

(peg:tree (match-pattern num "23.45"))
(peg:tree (match-pattern formula "asd23.3"))
(peg:tree (match-pattern formula "23+24"))
(peg:tree (match-pattern astring "\"hello\""))

(define (pma str) (peg:tree (match-pattern DQ str)))
(define (pma1 str) (peg:tree (match-pattern astring str)))
