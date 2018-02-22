(display "neoleo.scm intitalising")
(newline)

(define (guile-hi)
  (display "hello from guile")
  (newline))

(define *cell-formulae* (make-hash-table))

(define (cell-id row col)
  (+  col (* 1000 row)))

(define (set-cell-formula formula row col)
  (hash-set! *cell-formulae* (cell-id row col) formula))

(define (get-cell-formula row col)
  (hash-ref *cell-formulae* (cell-id row col)))

