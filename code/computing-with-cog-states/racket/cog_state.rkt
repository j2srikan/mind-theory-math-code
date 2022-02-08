#lang racket


 (define (binomial_coefficients n k)
 (if (and (>= n 0) (>= k 0) (>= n k))
      (if (or (= k 0) (= k n))
          1
          (+ (binomial_coefficients (- n 1) (- k 1))
             (binomial_coefficients (- n 1) k)))
      0))

(define (not-xor a b)
   (cond 
     [(= a b)
      1]
     [(not (= a b ))
      0]))

(not-xor 0 1)

(binomial_coefficients 5 2)