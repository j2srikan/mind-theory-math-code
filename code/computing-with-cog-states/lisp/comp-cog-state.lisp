;;;; Computing with Cognitive States
;;;; Implementing some of the material from Reiman's Arxiv article

    (ql:quickload "alexandria")
    (ql:quickload "trivia")
    
    (defpackage computing-with-cognitive-states
      (:use cl)
      (:nicknames :cwcs)
      (:local-nicknames (:alex :alexandria)))
    
    (in-package :cwcs)
    
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

(defun similarity (dimension dist &key (kappa 200.0))
  (exp (* -1.0 kappa (d-between dimension dist))))

(defun not-xor (a b)
	(trivia:match (list a b)
	  ('(1 1) 1)
	  ('(1 0) 0)
	  ('(0 1) 0)
	  ('(0 0) 1)))

(defun bind-memories (a b)
  (mapcar 'not-xor a b))

(defun rand-0/1-p (p) (if (< (random 1.0) p) 1 0))

(defun bundle-slot (a b &optional (p 0.5))
  (trivia:match (list a b)
    ('(1 1) 1)
    ('(1 0) (rand-0/1-p p))
    ('(0 1) (rand-0/1-p p))
    ('(0 0) 0)))

(defun bundle-memories (a b &optional (p 0.5))
  (mapcar (lambda (x y) (bundle-slot x y p)) a b))

(defun mk-rand-mem (dimension &optional (p 0.5))
  (do ((i 0 (+ 1 i))
       (outlist ()))
      ((> i dimension) outlist)
    (setq outlist
	  (cons (rand-0/1-p p) outlist))))
