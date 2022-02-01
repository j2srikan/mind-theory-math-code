#lang racket
(define (greeting)        ; defines piece as a function
  (print "hello world!"))

(greeting)

(struct cell ([content #:mutable]) #:transparent)
(define a-cell (cell 0))
(set-cell-content! a-cell 1)




(define (factorial n)
    (do ((counter n (- counter 1))
        (result 1 (* result counter)))
    ((= counter 0) result) ; Stop condition and return value.
    ; The body of the do-loop is empty.
    ))

(factorial 5)
