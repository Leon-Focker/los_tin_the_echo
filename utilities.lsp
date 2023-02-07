;; Utilities for feedback
(in-package :fb)

;; start
;;; syntactic sugar for (nth 0 start-times)
;;; start-times must of course be bound, else the compiler will complain :)
(defmacro startn (index)
  `(nth ,index start-times))

;; dynamic-collect
;;; give any amount of arguments to a collect in a loop in a macro :)
;;; see play-rhythm for actual use
(defmacro dynamic-collect (&rest rest)
  `(loop for i in (quote ,rest) collect 'collect collect i))
(defmacro dynamic-collect-instruments (instrument-calls)
  `(loop for i in ,instrument-calls collect 'collect collect `(funcall ,i)))

;; play-rhythm
;;; easy way to play rhythm with soundfiles
(defmacro fb-play (start-time end-time instrument-calls
		   &key (sfl *percussive*)
		     new-id ;(lambda () (ly::id (first (ly::data sfl)))))
		     sound
		     duration
		     (rhythm-list '(list 10))
		     rhythm-fun
		     (degree 45))
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
      do (incf time rhythm)))

;; EOF utilities.lsp
