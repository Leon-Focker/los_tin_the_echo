(in-package :feedback)

(defparameter *pattern1* '(0.5 0.75 0.5 0.75 0.5 0.75 0.75))
(defparameter *pattern2* '(0.5 0.75 0.75 0.5 0.75 0.5 0.75))
(defparameter *pattern3* '(0.75 0.5 0.75 0.5 0.75 0.5 0.5 0.75))
(defparameter *pattern4* '(1/2 2/3 1/2 1/3 1/3 1/3 3/4))


(defparameter *amp-env01*
  (append 
   (loop for i from 0 to 80 collect i collect (expt (/ i 80) 0.3))
   (loop for i from 81 to 100 collect i collect (expt (/ (- 100 i) 20) 0.3))))
;; EOF pattterns.lsp