(defpackage :neural-bifurcation
  (:nicknames :neubif)
  (:use :cl)
  (:import-from "EAZY-GNUPLOT"
		"WITH-PLOTS"
		"PLOT"
		"GP-SETUP"))


(defparameter c         20)
(defparameter gca       4.4)
(defparameter vca       120)
(defparameter gk        8)
(defparameter vk        -84)
(defparameter gl        2)
(defparameter vl        -60)
(defparameter v1        -1.2)
(defparameter v2        18)
(defparameter v3        2)
(defparameter v4        17)
(defparameter phi       0.04)
(defparameter tau-n     0.8)
(defparameter curr      85.0)
(defparameter time-step 0.05)

;; for parameters and some examples
;; https://github.com/dakota-hawkins/MathBio/blob/c31fc434a62cd0991d4ccc129339600a25ddfcbc/morris_lecar.m
;;  https://catalogimages.wiley.com/images/db/pdf/9781119951698.excerpt.pdf

(in-package :neubif)

(defun mss (v &key v1 v2)
  (* 0.5 (+ 1 (tanh (/ (- v v1) v2)))))

(defun nss (v &key v3 v4)
  (* 0.5 (+ 1 (tanh (/ (- v v3) v4)))))

(defun tau-n (v &key v3 v4 phi)
  (/ 1 (* phi (cosh (/ (- v v3) (* 2 v4))))))

(defun dndt (v n)
  (/ (- (nss v :v3 v3 :v4 v4) n) (tau-n v :v3 v3 :v4 v4 :phi phi)))

(defun dvdt (v n c curr gl gca gk vl vca vk)
  (/ (- curr (+ (* gl (- v vl)) (* gca (mss v :v1 v1 :v2 v2) (- v vca)) (* gk n (- v vk)))) c))

(defun dvdt-with-defaults (v n)
  (/ (- curr (+ (* gl (- v vl)) (* gca (mss v :v1 v1 :v2 v2) (- v vca)) (* gk n (- v vk)))) c))

(defun euler-update (old-value rate-of-change time-step)
  (+ old-value (* rate-of-change time-step)))

(defun one-step (v n-init)
  (list (euler-update v (dvdt-with-defaults v n-init) time-step) (euler-update n-init (dndt v n-init) time-step)))
  
(defun test-loop-morris-lescar (&key (vinit -60) (stop-time 1.0))
  (let ((temp (one-step vinit (nss vinit :v3 v3 :v4 v4))))
    (do* 
     ((vs (list (first temp) vinit))
      (time time-step (+ time time-step))
      (ts (list time-step 0))
      (ns (list (second temp)  (nss 0 :v3 v3 :v4 v4))))
     ((> time stop-time) (list (nreverse ts) (nreverse vs) (nreverse ns)))
      (destructuring-bind (v n) (one-step (car vs) (car ns))
	(push v vs)
	(push n ns)
	)
      (push time ts))))
      
      
     
	
(defun mandl-plot (output plot-data)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png
	      :key '())
    (plot
     (lambda ()
       (loop for times in (first plot-data)
	     for volts in (second plot-data)
	     do (format t "~&~a ~a" times volts)))
     :with '(:lines :title "Voltage" :lc "black" :lw 2))
    ;; (plot
    ;;  (lambda ()
    ;;    (loop for times in (first plot-data)
    ;; 	     for currs in (second plot-data)
    ;; 	     do (format t "~&~a ~a" times currs)))
    ;;  :with '(:lines :lw 2 :lc "red" :title "Current"))
    output))


(defun mandl-pp-plot (output plot-data)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :output output :terminal :png
	      :key '())
    (plot
     (lambda ()
       (loop for volts in (second plot-data)
	     for doubleus in (third plot-data)
	     do (format t "~&~a ~a" volts doubleus)))
     :with '(:lines :title "Phase Plane Plot" :lc "black" :lw 2))
    ;; (plot
    ;;  (lambda ()
    ;;    (loop for times in (first plot-data)
    ;; 	     for currs in (second plot-data)
    ;; 	     do (format t "~&~a ~a" times currs)))
    ;;  :with '(:lines :lw 2 :lc "red" :title "Current"))
    output))


