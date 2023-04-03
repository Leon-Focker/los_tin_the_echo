;; * morph.lsp
;;; home of morph-patterns and interpolate-patterns - two ways of transitioning
;;; from one pattern (any kind of list with numbers) to another

(in-package :sc)

;; ** patterns-to-rhythms-list
;;; list of style '((1 1 (1) 1 1 (1)) ((1) 1 1 (1) 1))
;;; to '((1 1 1 1 1 1) (1 1 1 1 1))
;;; could this be easier with sc::flatten?
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

;; ** mod1
;;; (mod x 1), but return 1 if (and (= 0 (mod x 1)) (not (= 0 x)))
(defun mod1 (x)
  (let ((x (rational x)))
    (cond ((= 0 x) 0)
	  ((= 0 (mod x 1)) 1)
	  (t (mod x 1)))))
(export 'mod1 :sc)

;; ** morph-patterns
;;; morph between two patterns, using a morphing-function,
;;; eg. fibonacci-transition. This morphing-function will determine which
;;; pattern to choose the next element from. The numerical values in patterns
;;; are not changed, but the patterns are mixed.
;;; patterns - a list of patterns - list of sublists containing durations.
;;;  Can also contain durations in parentheses (rests). Here is a simple example
;;;  for patterns:
;;;  '((.25 (.25) .25 (.25)) ((.25) .25 (.25) .25))
;;;  -> two patterns, each have a total duration of 1.
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
;;; cut-end? - t or nil, tries to avoid a repetition of an entire unaltered
;;;  pattern at the end of the sequence. Should be nil for most cases.
;;;  Also this currently only works when a list is given for the transition
;;; overlap-duration - t will append the last rhythm without squeezing it
;;;  perfectly into the given duration (no subtraction of last rhythm)
;;; length - when nil, generate a list with duration, when a number, generate
;;;  a list with this length.
;;; collect-indices - if nil, everything happens as normal. If t, don't collect
;;;  the rhythm value but a listwith two values - the first being the index of
;;;  the pattern in patterns and the second being the index of the rhythm value
;;;  in the pattern. This way, you could morph a second set of patterns
;;;  according to the original set of patterns. For example the first set of
;;;  patterns could be some rhythms and the second set could be note names.
;;;  These could normally not be processed by morph-patterns but can then be
;;;  morphed using the indices the call to morph-patterns with the rhythms
;;;  produced.
;;; morphing-function - determines how the transition between the patterns
;;;  is happening. Can be a list (eg. one made from fibonacci-transitions).
;;;  Can also be a function, which must accept one rational number as an
;;;  argument and returns one rational number.
(defun morph-patterns (patterns duration &optional
					   cut-end?
					   overlap-duration
					   length
					   collect-indices
					   (morphing-function
					    (fibonacci-transition 20)))
  ;; sanity checks
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
  (when (or length collect-indices) (setf overlap-duration t))
  #|(visualize 
  (loop for i below length collect (funcall morphing-function i)))|#
  ;; the important part:
  (let* ((rhythms-list (patterns-to-rhythms-list patterns)))
    (loop for i from 0
       for sum = 0 then (rational (+ sum (if (= 0 rhythm) 1 rhythm)))
       for key = (round (ly::mirrors (funcall morphing-function
					      (if length i sum))
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
       ;; Alternatively it's a list of the indices at which to find the event.
       for event = (if collect-indices `(,key ,index) (nth index pattern))
       until (if length (>= i (1- length)) (>= (+ sum rhythm) duration))
       ;; if collect-indices is t, collect lists, in which the first element is
       ;; the index of the pattern and the second element is the index within
       ;; that pattern. Else collect event (element in pattern at that position)
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

;; ** interpolate-patterns
;;; interpolate the numerical values in one pattern until it matches the next
;;; pattern. The patterns do not have to have the same length.
;;; patterns - list of sublists containing durations - see morph-patterns
;;; duration - the sum of all durations in the result
;;;  -> the duration of the resulting sequence
;;; transition-ratios - the duration of the interpolation between one pattern
;;;  and the next, relative to the duration of the result. This must be a list
;;;  with one less item than the patterns argument, so if you want to morph
;;;  three patterns, transition-ratios would initially be '(1 1) but could be
;;;  set to any list containing any two numbers.
(defun interpolate-patterns (patterns duration
			     &optional overlap-duration transition-ratios)
  ;; sanity checks
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
    ;; the fun part:
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
