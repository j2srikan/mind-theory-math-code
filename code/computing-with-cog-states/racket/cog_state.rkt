#lang racket
(require math/distributions)
(define kappa 2)
(define N 40.0)
(define p 0.5)
(random-seed 2)

(define dis (binomial-dist N p))


(define (similarity d)
  (define cdf-value (cdf dis d))
  (exp (- (* kappa cdf-value))))

(define (not-xor a b)
   (cond 
     [(= a b)
      1]
     [(not (= a b ))
      0]))
(define (xor a b)
   (cond 
     [(= a b)
      0]
     [(not (= a b ))
      1]))
(define (mySum L)
  (apply + L))

(define (hamDis a b)
  (mySum (vector->list(vector-map xor a b))))



  
(define (randOne p)
 (if (< (random) p) 1 0))

(define (bundle_each a b)
   (cond 
     [(and (= a 0)(= a b))
      0]
      [(and (= a 1)(= a b))
      1]
     [(not (= a b ))
      (randOne 0.5)]))

(define (bundle a b)
   (vector-map bundle_each a b))


(define (left_bundle L)
  (if ( = 2 (length L))
      (bundle (list-ref L 0)(list-ref L 1))
      (bundle (first L)(left_bundle (rest L)))))

(define (right_bundle L)
  (if ( = 2 (length L))
      (bundle (list-ref L 0)(list-ref L 1))
      (bundle (last L)(right_bundle (reverse (rest (reverse L)))))))

(define vm1 (vector 1 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0))
(define vm2 (vector 1 1 0 0 0 0 1 0 1 0 1 0 0 0 1 1 0 0 0 0 1 0 1 0 1 0 0 0 0 0))
(define vm3 (vector 1 1 1 1 0 0 0 0 0 0 1 0 1 1 1 1 1 1 0 0 0 0 0 0 1 0 1 1 0 0))
(define vm4 (vector 1 1 0 1 1 0 0 1 0 1 0 0 1 1 1 1 0 1 1 0 0 1 0 1 0 0 1 1 1 1))
(define vm5 (vector 1 1 1 1 1 0 1 0 0 0 1 0 1 0 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 1))
(define vm6 (vector 1 0 1 1 1 0 1 1 0 1 0 0 1 0 1 0 1 1 1 0 1 1 1 1 0 0 1 0 1 1))
(define vm7 (vector 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1))

(define my_memory (list vm1 vm2 vm3 vm4 vm5 vm6 vm7))

(define left_B (left_bundle my_memory))
(define right_B (right_bundle my_memory))

(define left_results (vector (hamDis left_B vm1)(hamDis left_B vm2)(hamDis left_B vm3) (hamDis left_B vm4) (hamDis left_B vm5)(hamDis left_B vm6)(hamDis left_B vm7)))
(define right_results (vector (hamDis right_B vm1)(hamDis right_B vm2)(hamDis right_B vm3)(hamDis right_B vm4)(hamDis right_B vm5)(hamDis right_B vm6)(hamDis right_B vm7)))

(define (divD a)
  (/ a N)
)

(vector-map divD left_results )

(vector-map divD right_results )
