#lang racket

(define (twice f v)
  (f (f v)))


(define (divide v)
   (/ v 2))

; sqrt(81) = 9 , sqrt(9) = 3
(twice sqrt 81)

; 16/2 = 8, 8/2 =4
(twice divide 16)

(define counter 6)
(define factorial 1)
(let loop ()
    (set! factorial (* factorial counter))
    (set! counter (sub1 counter))
    (displayln factorial)
    (when (> counter 0) (loop)))
(displayln factorial)
