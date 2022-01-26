#lang racket

(define (twice function v)
  (function (function v)))


(define (divide v)
   (/ v 2))

; sqrt(81) = 9 , sqrt(9) = 3
(twice sqrt 81)

; 16/2 = 8, 8/2 =4
(twice divide 16)

