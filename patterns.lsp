;; * patterns.lsp
;;; the patterns in here are not really used... but let's keep them for now!
;;; also some envelopes are defined here. I guess all of this could be merged
;;; with the ** material part of score.lsp

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

(defun env-fun1 (breakpoint &optional (exponent 0.3))
  (let ((bp (max 0 (min 100 (round breakpoint)))))
    (append
     (if (= bp 0) '(0 1)
	 (loop for i from 0 to bp
	    collect i collect (expt (/ i bp) exponent)))
     (loop for i from (1+ bp) to 100
	collect i collect (expt (/ (- 100 i) (- 100 bp)) exponent)))))

;; base should be between 0 and 1
(defun env-expt (pow &optional (base 0) reverse? flip?)
  (unless (<= 0 base 1)
    (warn "base should be between or equal to 0 and 1 but is: ~a" base))
  (loop for i from 0 to 100
     for val = (expt (/ (if reverse? (- 100 i) i) 100) pow)
     collect i collect
       (+ (* (- 1 base) (if flip? (- 1 val) val))
	  base)))

;; srt-env function
(defun srt-break (breakpoint new-val &optional (br-len .001))
  (unless (and (numberp breakpoint) (numberp new-val))
    (error "all arguments in srt-break should be numbers"))
  (cond ((<= breakpoint 0) `(0 ,new-val 100 ,new-val))
	((>= breakpoint (- 100 br-len)) `(0 0 100 0))
	(t `(0 0 ,breakpoint 0 ,(+ breakpoint br-len) ,new-val 100 ,new-val))))

(defun srt-break2 (breakpoint old-val &optional (br-len .001))
  (unless (and (numberp breakpoint) (numberp old-val))
    (error "all arguments in srt-break should be numbers"))
  (cond ((<= breakpoint 0) `(0 0 100 0))
	((>= breakpoint (- 100 br-len)) `(0 ,old-val 100 ,old-val))
	(t `(0 ,old-val ,breakpoint ,old-val ,(+ breakpoint br-len) 0 100 0))))

;; this is stupid and useless:
#+nil(defun srt-env (first-val before-break after-break last-val break-point
		&optional (break-length 0))
  (unless (and (numberp before-break) (numberp after-break)
	       (numberp first-val) (numberp last-val)
	       (numberp break-point) (numberp break-length))
    (error "all arguments in srt-env should be numbers"))
  (unless (<= 0 break-point 100) (error "breakpoint is out of bounds"))
  (when (<= break-length 0) (setf break-length 0.001))
  (let* ((nd (+ break-point break-length)))
    (loop for i from 0 to 100
       with flag-st
       with flag-nd
       when flag-st collect break-point
       when flag-st collect before-break
       when flag-nd collect nd
       when flag-nd collect after-break
       collect i
       when (< i break-point) collect before-break
       when (> i break-point) collect after-break
       when (<= break-point i nd) collect
	 (+ (* (/ (- i break-point) break-length) after-break)
	    (* (- 1 (/ (- i break-point) break-length))
	       before-break))
       do (setf flag-st (and (<= break-point (1+ i) (1+ break-point))
			     (not (= 0 (mod break-point 1))))
		flag-nd (and (<= nd (1+ i) (1+ nd))
			     (not (= 0 (mod nd 1))))))))

;; EOF pattterns.lsp
