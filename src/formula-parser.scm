(use-modules (ice-9 peg string-peg))
(use-modules (ice-9 peg using-parsers))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 format))


(define-peg-string-patterns
  "formula <-- func / num
  func <-- funct '(' num ')'
  funct <-- [a-z]+
  num <-- [0-9]+
  ")


(define (show str)
  (format #t "\nParsing: ~s\n" str)
  (pretty-print (peg:tree (match-pattern formula str))))

(show "hello(12)")
(show "42")
