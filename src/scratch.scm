;;; tests on pattern matching

(use-modules (ice-9 match))

(match (list 12 "+")
       ((op1 "+" op2 . rest ) (cons op2 rest)))

(match (list 12 "+" 13)
       ((op1 "+" op2 . rest ) (cons op2 rest)))



(match (list 12 "+")
       ((op1 "+" op2 . rest ) (cons op2 rest))
       (_ 'nada))

