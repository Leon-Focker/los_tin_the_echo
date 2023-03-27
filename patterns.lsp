(in-package :feedback)

(defparameter *pattern1* '(0.5 0.75 0.5 0.75 0.5 0.75 0.75))
(defparameter *pattern2* '(0.5 0.75 0.75 0.5 0.75 0.5 0.75))
(defparameter *pattern3* '(0.75 0.5 0.75 0.5 0.75 0.5 0.5 0.75))
(defparameter *pattern4* '(1/2 2/3 1/2 1/3 1/3 1/3 3/4))
(defparameter *pattern5* '(0.2 0.45 0.2 0.5 0.45))
(defparameter *pattern6* '(1 1/6 1/6 0.2 1/3))
(defparameter *pattern7* '(0.2 0.2 0.4))


(defparameter *amp-env01*
  (append 
   (loop for i from 0 to 80 collect i collect (expt (/ i 80) 0.3))
   (loop for i from 81 to 100 collect i collect (expt (/ (- 100 i) 20) 0.3))))

(defun env-fun1 (breakpoint)
  (let ((bp (max 0 (min 100 (round breakpoint)))))
    (append
     (if (= bp 0) '(0 1)
	 (loop for i from 0 to bp
	    collect i collect (expt (/ i bp) 0.3)))
     (loop for i from (1+ bp) to 100
	collect i collect (expt (/ (- 100 i) (- 100 bp)) 0.3)))))

;; base should be between 0 and 1
(defun env-expt (pow &optional (base 0) reverse? flip?)
  (unless (<= 0 base 1)
    (warn "base should be between or equal to 0 and 1 but is: ~a" base))
  (loop for i from 0 to 100
     for val = (expt (/ (if reverse? (- 100 i) i) 100) pow)
     collect i collect
       (+ (* (- 1 base) (if flip? (- 1 val) val))
	  base)))

(defparameter *amp-env02*
  '(0 0  100 0))

;; EOF pattterns.lsp
