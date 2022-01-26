;;;; Computing with Cognitive States
;;;; Implementing some of the material from Reiman's Arxiv article

(ql:quickload "alexandria")
(ql:quickload "trivia")
(defpackage computing-with-cognitive-states
  (:use cl)
  (:nicknames :cwcs)
  (:local-nicknames (:alex :alexandria)))

(in-package :cwcs)


;;; Computing Similarity Reiman argues that similarity should be a
;;; _global_ property and depend on the number of vectors in between.
;;; I tried to compute this by looking at how many vectors were 1, 2,
;;; ... changes away from a candidate (denominator), and 
;;; how many of them would involve changes in one of the "d"
;;; mismatched slots (numerator), then the probability is the ratio.
;;; This leads to a small number for nearby and a big number from many
;;; different slots

(defun d-between (dist dimension)
  (do ((my-d (- dist 1) (- my-d 1))
       (my-dimension (- dimension 1) (- my-dimension 1))
       (accum (/ dist dimension) (* accum (/ my-d my-dimension))))
      ((equal my-d 1))
    (print (format t "my is ~a and my accum is ~a~%" my-d accum))))

(defun sum-denom (dimension dist)
  (do ((counter 1 (+ 1 counter))
       (accum (alex:binomial-coefficient dimension dist)
	      (+ accum (alex:binomial-coefficient
			dimension counter))))
      ((>= counter dist) accum)))

(defun sum-num (dist)
  (do* ((counter 1 (+ 1 counter))
       (accum (alex:binomial-coefficient dist counter)
	      (+ accum (alex:binomial-coefficient
			dist counter))))
      ((>= counter dist) accum)))

(defun d-between (dimension dist)
  (/ (sum-num dist) (sum-denom dimension dist) )) 

(defun similarity (dimension dist &optional (kappa 1.0))
  (exp (* -1.0 kappa (d-between dimension dist))))

(defun not-xor (a b)
	(trivia:match (list a b)
	  ('(1 1) 1)
	  ('(1 0) 0)
	  ('(0 1) 0)
	  ('(0 0) 1)))

(defun bind-memories (a b)
  (mapcar 'not-xor a b))


;;(defun bundle-memories)

