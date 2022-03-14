(ql:quickload "swank")

(swank:create-server :port 4123)

(defun bad-add (a b)
  (- a b))

(dotimes (i 500)
  (format t "~&~a + ~a is ~a" 4 5 (bad-add 4 5))
  (sleep 5))
