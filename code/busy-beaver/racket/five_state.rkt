#lang racket

; Chris Longley Sixuan Chen
(struct turing-machine ([state #:mutable] [tape #:mutable] [head-location #:mutable] ))

(define (move-left tm)
  (let ((loc (turing-machine-head-location tm))
	  (lst (turing-machine-tape tm)))
   (if (= loc 0)
          (set-turing-machine-tape! tm (cons 0 lst))
          (set-turing-machine-head-location! tm (- loc 1))
	)
 ))
 (define (move-right tm)
   (let ((loc (turing-machine-head-location tm))
	  (lst (turing-machine-tape tm)))
     (when (= (+ loc 1) (length lst))
	(set-turing-machine-tape! tm (append lst (list 0))))
     (set-turing-machine-head-location! tm (+ loc 1))))


 (define (move tm dir)
   (cond 
     [(eq? dir 'left)
      (move-left tm)]
     [(eq? dir 'right)
      (move-right tm)]))

(define (equal-sv tm s v)
  (and (eq? (turing-machine-state tm) s)
	 (= (list-ref (turing-machine-tape tm) (turing-machine-head-location tm)) v)))


(define (rule tm) ;;state value
  (cond
    [(equal-sv tm 'a 0)
     (set-turing-machine-state! tm 'b)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'right)]
    [(equal-sv tm 'a 1)
     (set-turing-machine-state! tm 'c)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
      (move tm 'left)]
    [(equal-sv tm 'b 0)
     (set-turing-machine-state! tm 'c)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'right)]
    [(equal-sv tm 'b 1)
     (set-turing-machine-state! tm 'b)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'right)]
    [(equal-sv tm 'c 0)
     (set-turing-machine-state! tm 'd)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'right)]
    [(equal-sv tm 'c 1)
     (set-turing-machine-state! tm 'e)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 0))
     (move tm 'right)]
    [(equal-sv tm 'd 0)
     (set-turing-machine-state! tm 'a)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'left)]
    [(equal-sv tm 'd 1)
     (set-turing-machine-state! tm 'c)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'left)]
      [(equal-sv tm 'e 0)
     (set-turing-machine-state! tm 'h)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 1))
     (move tm 'left)]
    [(equal-sv tm 'e 1)
     (set-turing-machine-state! tm 'a)
     (set-turing-machine-tape! tm (list-set (turing-machine-tape tm) (turing-machine-head-location tm) 0))
     (move tm 'left)]
    )
 
  )




(define (pp-tm tm)
  (format "state:~a, tape: ~a, head: ~a" (turing-machine-state tm) (turing-machine-tape tm) (turing-machine-head-location tm)))

(define tm (turing-machine 'a (list 0) 0))
(pp-tm tm)
(define (runOne tm)
(rule tm)
(pp-tm tm)
  )
;This is to demonstrate the intermediate results
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)
(runOne tm)

; Demonstration End

; this the while loop but it failed to produce the intermedia results but it works

(define tm2 (turing-machine 'a (list 0) 0))
(pp-tm tm2)
(let loop()
    (rule tm2)
    (pp-tm tm2)
    (when (eq? #f (eq? (turing-machine-state tm2) 'h))(loop))
)

(pp-tm tm2)
  

