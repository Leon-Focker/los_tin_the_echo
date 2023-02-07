(in-package :sc)
;; for use in instrumental piece, consider .../instrumental/helpers.lsp

;; * patterns-to-rhythms-list
;;; list of style '((1 1 (1) 1 1 (1)) ((1) 1 1 (1) 1))
;;; to '((1 1 1 1 1 1) (1 1 1 1 1))
(defun patterns-to-rhythms-list (patterns)
  (loop for pattern in patterns
	collect
	(loop for i in pattern
	      collect
	      (let ((rhythm (if (listp i)
				(car i)
				i)))
		(if (numberp rhythm)
		    rhythm
		    (error "pattern holds weird value: ~a" rhythm))))))

;; * mod1
(defun mod1 (x)
  (cond ((= 0 x) 0)
	((= 0 (mod x 1)) 1)
	(t (mod x 1))))

;; * morph-patterns
;;; morph between two patterns, using a morphing-function,
;;; eg. fibonacci-transition
;;; patterns - list of sublists containing durations - a list of patterns
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
;;; morphing-function - determines how the transition between the patterns
;;;  is happening. Can be a list (eg. one made from fibonacci-transitions).
;;;  Can also be a function, which must accept one rational number as an
;;;  argument and returns one rational number.
;;; cut-end? - t or nil, tries to avoid a repetition of an entire unaltered
;;;  pattern at the end of the sequence. Should be nil for most cases.
;;;  Also this currently only works when a list is given for the transition
(defun morph-patterns (patterns duration &optional
					   cut-end?
					   (morphing-function (fibonacci-transition 20)))
  (unless (typep patterns 'list)
    (error "morph-patterns needs patterns to be a list: ~a" patterns))
  (unless (> duration 0)
    (error "for morph-patterns, duration must be greater than 0: ~a" duration))
  (unless (typep morphing-function 'function)
    (if (typep morphing-function 'list)
	(setf morphing-function
	      (ly::make-list-into-function morphing-function
				       (+ duration (if cut-end?
						       (length (second patterns))
						       0))))
	(error "morphing-function must either be of type function or list: ~a"
	       morphing-function)))
  (unless (numberp (funcall morphing-function 0))
    (error "morphing function not usefull: ~a" morphing-function))
  (let* ((rhythms-list (patterns-to-rhythms-list patterns)))
    (loop
      for sum = 0 then (+ sum rhythm)
      for key = (round (ly::mirrors (funcall morphing-function sum) 0 (length patterns)))
      for pattern = (nth key patterns)
      for rhythms = (nth key rhythms-list)
      ;; position relative to the pattern
      for index = (ly::decider (let ((pattern-len (loop for i in rhythms sum i)))
				 (rescale (+ (mod sum pattern-len)
					     (expt 10.0d0 -8)) ; to combat float-errors i guess
					  0
					  pattern-len
					  0
					  1))
			       rhythms)
      ;; rhythm is the duration of the event
      for rhythm = (nth index rhythms)
      ;; event can be a rest or a note with a duration
      for event = (nth index pattern)
      until (>= (+ sum rhythm) duration)
      collect event into ls
      ;; when the next rhythm would complete the sequence:
      finally
	 ;; add a difference, to bring sequence to its max length
	 ;; if the rhythm-value was a rest, the added difference will
	 ;; too be a rest
	 (let* ((difference (rationalize (- duration sum))))
	   (unless (sc::equal-within-tolerance difference 0)
	     (if (atom event)		 
		 (setf ls (append ls (list difference)))
		 (setf ls (append ls (list (list difference)))))))
	 ;; return the final rhythm sequence:
	 (return ls))))

;; * interpolate-patterns
;;; slowly adjust the durations of a pattern until it matches the next one
;;; patterns - list of sublists containing durations - a list of patterns
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
(defun interpolate-patterns (patterns duration)
  (unless (typep patterns 'list)
    (error "morph-patterns needs patterns to be a list: ~a" patterns))
  (unless (> duration 0)
    (error "for morph-patterns, duration must be greater than 0: ~a" duration))
  (loop for i from 0
	for sum = 0 then (+ sum rhythm)
	for interp = (* (/ sum duration) (1- (length patterns)))
	for pattern1 = (nth (floor interp) patterns)
	for pattern2 = (nth (ceiling interp) patterns)
	for event1 = (nth (mod i (length pattern1)) pattern1)
	for event2 = (nth (mod i (length pattern2)) pattern2)
	for rhythm = (+ (* (- 1 (mod1 interp))
			   (if (listp event1) (car event1) event1))
			(* (mod1 interp)
			   (if (listp event2) (car event2) event2)))
	for event = (if (listp (if (>= (mod1 interp) 0.5)
				   event2
				   event1))
			(list rhythm)
			rhythm)
     until (>= (+ sum rhythm) duration)
	collect event into ls
	finally
	   ;; add a difference, to bring sequence to its max length
	   ;; if the rhythm-value was a rest, the added difference will
	   ;; too be a rest
	   (let* ((difference (rationalize (- duration sum))))
	     (unless (sc::equal-within-tolerance difference 0)
	       (if (atom event)		 
		   (setf ls (append ls (list difference)))
		   (setf ls (append ls (list (list difference)))))))
	   ;; return the final rhythm sequence:
       (return ls)))

(export '(slippery-chicken morph-patterns interpolate-patterns))

;; EOF morph.lsp
