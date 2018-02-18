(use-modules (ice-9 peg string-peg))

(define-peg-string-patterns
  "passwd <-entry* !.
  entry <-- (! NL .)* NL*
  NL < '\n'")
