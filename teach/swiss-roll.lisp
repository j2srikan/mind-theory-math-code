(ql:quickload "alexandria" :silent t)
(ql:quickload "eazy-gnuplot" :silent t)

(defpackage #:swiss-roll
  (:nicknames "swiss")
  (:use #:cl #:eazy-gnuplot))

(in-package :swiss-roll)

(defun 2d-gaussian-random (&key (how-many 1) (m1 0.0) (m2 0.0) (sd1 1.0) (sd2 1.0))
  (let ((out 'nil))
    (dotimes (i how-many out)
      (multiple-value-bind (x y) (alexandria:gaussian-random)
	(setq out (cons (list (+ m1 (* sd1 x)) (+ m2 (* sd2 y))) out))))))

(defun swiss-roll-transform (x y)
  (list (* x (cos x)) y (* x (sin x))))

(defun swiss-roll-dat (&key (how-many 1) (m1 0.0) (m2 0.0) (sd1 1.0) (sd2 1.0))
  (let ((2dgdat (2d-gaussian-random :how-many how-many :m1 m1 :m2 m2 :sd1 sd1 :sd2 sd2)))
    (mapcar (lambda (x) (destructuring-bind (a b) x
			  (swiss-roll-transform a b)))
	    2dgdat)))

;;; numbers taken from: https://people.cs.uchicago.edu/~dinoj/manifold/swissroll.html
(defun make-example-swiss-roll ()
  (let ((mredx 7.5)
	(mredy 7.5)
	(mbluex 7.5)
	(mbluey 12.5)
	(mgreenx 12.5)
	(mgreeny 7.5)
	(mblackx 12.5)
	(mblacky 12.5)
	(how-many 400))
    (list (swiss-roll-dat :how-many how-many :m1 mredx :m2 mredy)
	  (swiss-roll-dat :how-many how-many :m1 mbluex :m2 mbluey)
	  (swiss-roll-dat :how-many how-many :m1 mgreenx :m2 mgreeny)
	  (swiss-roll-dat :how-many how-many :m1 mblackx :m2 mblacky))))


(defparameter *srd* (make-example-swiss-roll))


(defun swiss-plot (output plot-data)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png)
    (splot
     (lambda ()
       (loop for p in plot-data
	     do (format t "~&~a ~a ~a" (first p) (second p) (third p))))
    :with '(:points :pt 5 :lc :rgb "blue")))
  output)

(defun swiss-plot-all (output plot-data &optional (flat nil))
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png)
    (gp :set :title "Swiss Roll Data")
;    problem with the list passed to view, cannot handle comma
					;    (gp :set :view '(60.0d0 #\U+002C 30.0d0))
    (when flat (gp :set :view 'map))
    (loop for ps in plot-data
	  for pt in (list 1 2 3 4)
	  for pc in (list "red" "green" "blue" "black")
	  do (splot
	      (lambda ()
		(loop for p in ps
		      do (format t "~&~a ~a ~a" (first p) (second p) (third p))))
	      :with `(:points :pt ,pt :lc :rgb ,pc))))
  output)

