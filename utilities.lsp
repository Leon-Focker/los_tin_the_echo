;; * Utilities for feedback
(in-package :fb)

;; ** get-start-times
;;; gat start times from a list of durations
(defun get-start-times (list-of-durations)
  (append '(0) (loop for i in list-of-durations sum i into sum collect sum)))

;; ** startn
;;; syntactic sugar for (nth 0 start-times)
;;; start-times must of course be bound, else the compiler will complain :)
(defmacro startn (index)
  `(nth ,index start-times))

;; ** avoid-repetition
;;; in a list of elements, when an element appears twice in a row it is cut
(defun avoid-repetition (ls &optional within-tolerance?)
  (loop for i in ls with last unless (if within-tolerance?
					 (equal-within-tolerance last i 1.0d-5)
					 (equal last i))
     collect i do (setf last i)))

;; ** get-durations
;;; collect the durations in a certain section of a certain layer
(defun get-durations (list-of-start-times)
  (loop for time in (avoid-repetition (sort list-of-start-times #'<) t)
     with last = 0
     collect (- time last)
     do (setf last time)))

;; ** get-durations-within
;;; collect the durations in a certain section of a certain layer
(defun get-durations-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect i))

;; ** get-start-times-within
;;; collect the start-times in a certain section of a certain layer
(defun get-start-times-within (durations min max)
  (loop for i in durations sum i into sum while (< sum max)
     when (> sum min) collect sum))

;; ** dynamic-collect
;;; give any amount of arguments to a collect in a loop in a macro :)
;;; see play-rhythm for actual use
(defmacro dynamic-collect (&rest rest)
  `(loop for i in (quote ,rest) collect 'collect collect i))
(defmacro dynamic-collect-instruments (instrument-calls)
  `(loop for i in ,instrument-calls collect 'collect collect `(funcall ,i)))

;; ** play-rhythm
;;; easy way to play rhythm with soundfiles
;;; do we really use rhythm-lists tho? or is it replaced by rhythm-fun
(defmacro fb-play (start-time end-time instrument-calls
		   &key (sfl *percussive*)
		     new-id ;(lambda () (ly::id (first (ly::data sfl)))))
		     sound
		     duration
		     (rhythm-list '(list 10))
		     rhythm-fun
		     (degree 45)
		     (printing t))
  `(loop for i from 0
      with time = ,start-time
      while (<= time ,end-time)
      for line = (/ (- time ,start-time) (- ,end-time ,start-time))
      for new-id = (unless ,sound
		     (if ,new-id
			 (funcall ,new-id)
			 (ly::id (first (ly::data ,sfl)))))
      for sound = (or ,sound
		      (loop for sf in (ly::data ,sfl) until (eq (ly::id sf) new-id)
			 finally (return sf)))
      for file = (ly::path sound)
      for duration = ,duration
      for rhythm = (or ,rhythm-fun
		       (nth (mod i (length ,rhythm-list)) ,rhythm-list))
      for degree = ,degree
	,@(dynamic-collect-instruments instrument-calls)
      do (when ,printing
	  (format t "~&~a" (ly::id sound))
	  (format t "~&start-time: ~a" time)
	  (format t "~&rhythm: ~a" rhythm)
	  (format t "~& ~&next sound:"))
	(incf time rhythm)))

;; EOF utilities.lsp
