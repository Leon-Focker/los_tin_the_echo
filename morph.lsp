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
  (let ((x (rational x)))
    (cond ((= 0 x) 0)
	  ((= 0 (mod x 1)) 1)
	  (t (mod x 1)))))

;; * morph-patterns
;;; morph between two patterns, using a morphing-function,
;;; eg. fibonacci-transition
;;; patterns - list of sublists containing durations - a list of patterns
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
;;; cut-end? - t or nil, tries to avoid a repetition of an entire unaltered
;;;  pattern at the end of the sequence. Should be nil for most cases.
;;;  Also this currently only works when a list is given for the transition
;;; overlap-duration - t will append the last rhythm without squeezing it
;;;  perfectly into the given duration (no subtraction of last rhythm)
;;; morphing-function - determines how the transition between the patterns
;;;  is happening. Can be a list (eg. one made from fibonacci-transitions).
;;;  Can also be a function, which must accept one rational number as an
;;;  argument and returns one rational number.
;;; length - when nil, generate a list with duration, when a number, generate
;;;  a list with this length.
(defun morph-patterns (patterns duration &optional
					   cut-end?
					   overlap-duration
					   length
					   (morphing-function
					    (fibonacci-transition 20)))
  (unless (typep patterns 'list)
    (error "morph-patterns needs patterns to be a list: ~a" patterns))
  (unless (or (not length) (numberp length))
    (error "length in morph-patterns should either be nil or a number: ~&~a"
	   length))
  (unless (> (or length duration) 0)
    (error "for morph-patterns, duration or length must be greater than 0: ~a"
	   (or length duration)))
  (unless (typep morphing-function 'function)
    (if (typep morphing-function 'list)
	(setf morphing-function
	      (ly::make-list-into-function
	       morphing-function
	       (or length
		   (+ duration (if cut-end?
				   (length (last patterns))
				   0)))))
	(error "morphing-function must either be of type function or list: ~a"
	       morphing-function)))
  (unless (numberp (funcall morphing-function 0))
    (error "morphing function not usefull: ~a" morphing-function))
  (when length (setf overlap-duration t))
  ;;(visualize (print (loop for i below length collect (funcall morphing-function i))))
  (let* ((rhythms-list (patterns-to-rhythms-list patterns)))
    (loop for i from 0
       for sum = 0 then (rational (+ sum (if (= 0 rhythm) 1 rhythm)))
       for key = (round (ly::mirrors (funcall morphing-function sum)
				     0 (1- (length patterns))))
       for pattern = (nth key patterns)
       for rhythms = (nth key rhythms-list)
       ;; position relative to the pattern
       for index = (let ((pat-dur (rational (loop for i in rhythms sum i))))
		     (if (not (= pat-dur 0))
			 (decider (rescale (mod sum pat-dur)
;;; to combat float-errors, maybe add (expt 10.0d0 -8)
					   0
					   pat-dur
					   0
					   1)
				  rhythms)
			 0))
       ;; rhythm is the duration of the event
       for rhythm = (nth index rhythms)
       ;; event can be a rest or a note with a duration
       for event = (nth index pattern)
       until (if length (>= i (1- length)) (>= (+ sum rhythm) duration))
       collect event into ls
       ;; when the next rhythm would complete the sequence:
       finally
       ;; add a difference, to bring sequence to its max length
       ;; if the rhythm-value was a rest, the added difference will
       ;; too be a rest
	 (let* ((difference (rational (- duration sum))))
	   (unless (sc::equal-within-tolerance difference 0)
	     (cond (overlap-duration (setf ls (append ls (list event))))
		   ((atom event) (setf ls (append ls (list difference))))
		   (t (setf ls (append ls (list (list difference))))))))
       ;; return the final rhythm sequence:
	 (return ls))))

;; * interpolate-patterns
;;; slowly adjust the durations of a pattern until it matches the next one
;;; patterns - list of sublists containing durations - a list of patterns
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
;;; transition-ratios - the duration of the interpolation between one pattern
;;;  and the next, relative to the duration of the result. This must be a list
;;;  with one less item than the patterns argument, so if you want to morph
;;;  three patterns, transition-ratios would initially be '(1 1) but could be
;;;  set to any list containing any two numbers.
(defun interpolate-patterns (patterns duration
			     &optional overlap-duration transition-ratios)
  (if transition-ratios
      (unless (= (1- (length patterns)) (length transition-ratios))
	(error "different number of pattern-transitions and transition-ratios ~
                in interpolate-patterns: ~a, ~a"
	       (1- (length patterns)) (length transition-ratios)))
      (setf transition-ratios (loop repeat (1- (length patterns)) collect 1)))
  (unless (typep patterns 'list)
    (error "morph-patterns needs patterns to be a list: ~a" patterns))
  (unless (> duration 0)
    (error "for morph-patterns, duration must be greater than 0: ~a" duration))
  (let* ((ratio-sum (loop for i in transition-ratios sum i))
	 (trans-durs (loop for i in transition-ratios
			collect (* (/ i ratio-sum) duration)))
	 (trans-starts (append '(0) (loop for i in trans-durs
				       sum i into sum collect sum))))
    (loop for i from 0
       for sum = 0 then (rational (+ sum rhythm))
       for n = (decider (/ sum duration) trans-durs)
       for interp = (rational (/ (- sum (nth n trans-starts))
				 (- (nth (1+ n) trans-starts)
				    (nth n trans-starts))))
       for pattern1 = (nth n patterns)
       for pattern2 = (nth (1+ n) patterns)
       for event1 = (nth (mod i (length pattern1)) pattern1)
       for event2 = (nth (mod i (length pattern2)) pattern2)
       for rhythm = (rational (+ (* (- 1 (mod1 interp))
				    (if (listp event1) (car event1) event1))
				 (* (mod1 interp)
				    (if (listp event2) (car event2) event2))))
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
	 (let* ((difference (rational (- duration sum))))
	   (unless (sc::equal-within-tolerance difference 0)
	     (cond (overlap-duration (setf ls (append ls (list event))))
		   ((atom event) (setf ls (append ls (list difference))))
		   (t (setf ls (append ls (list (list difference))))))))
       ;; return the final rhythm sequence:
	 (return ls))))

(export '(slippery-chicken
	  morph-patterns
	  interpolate-patterns))

;; EOF morph.lsp
