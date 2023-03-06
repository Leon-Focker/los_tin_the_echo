;; * Utilities for feedback
(in-package :fb)

;; ** get-start-times
;;; gat start times from a list of durations
(defun get-start-times (list-of-durations)
  (append '(0) (loop for i in list-of-durations sum i into sum collect sum)))

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
  (let ((ls (avoid-repetition (sort list-of-start-times #'<) t)))
    (loop for time in (cdr ls)
       with last = (car ls)
       collect (- time last)
       do (setf last time))))

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

;; ** scale-list-to-sum
;;; scale the elements in a list so that the sum of that list is *argument*
(defun scale-list-to-sum (ls &optional (target-sum 1))
  (let* ((sum (loop for i in ls sum i))
	 (scaler (/ target-sum sum)))
    (loop for i in ls collect (* i scaler))))

;; ** insert
;;; insert an element into list at index
(defun insert (ls index newelt)
  (if (= 0 index) (push newelt ls) (push newelt (cdr (nthcdr (1- index) ls))))
  ls)

;; ** insert-multiple
;;; insert multiple elements on multiple indices in a list
;;; the same index should only be used once for now
(defun insert-multiple (ls index-list newelt-list)
  (let* ((len (length newelt-list)))
    (unless (= (length index-list) len)
      (error "both index-list and newelt-list in insert-multiple should have ~
              the same length"))
    (loop for i from 0 and el in ls
       for mem = (member i index-list)
       when mem collect (nth (- len (length mem)) newelt-list)
       collect el)))
	   
;; ** dynamic-collect
;;; give any amount of arguments to a collect in a loop in a macro :)
;;; see play-rhythm for actual use
(defmacro dynamic-collect (&rest rest)
  `(loop for i in (quote ,rest) collect 'collect collect i))
(defmacro dynamic-collect-instruments (instrument-calls)
  `(loop for i in ,instrument-calls collect 'collect collect `(funcall ,i)))

;; ** name-var
(defmacro name-var (name i)
  `(if (<= ,i 1) ,name (read-from-string (format nil "~a~a" ,name ,i))))

;; ** name-var-highest
(defmacro name-var-highest (name i arg-list)
  `(let* ((ass (assoc ,name ,arg-list))
	  (len (1- (length ass)))
	  (n (min ,i len)))
     (if (<= n 1) ,name (read-from-string
			  (format nil "~a~a" ,name n)))))

#+nil(defmacro name-var (name i)
  `(if (= ,i 1) ,name (intern (format nil "~a~a" ,name ,i))))

;; ** get-loop-vars
;;; return a list with statements of type 'for var-name = var-def'
;;; arg-list - list of lists, these contain a name for a variable
;;;  as first element, then how those varable is defined in the loop
;;;  if there is more than one definition (eg. the sublist is longer
;;;  than 2 elements, (1- length) variables will be created. The first
;;; variable is always name var-name, after that: var-name2, var-name3...
;;; EXAMPLE
#|
(get-loop-vars '((rythm (nth i ls)) () (sound 4 5)))
=> (FOR RYTHM = (NTH I LS) FOR SOUND = 4 FOR SOUND2 = 5)
|#
(defmacro get-loop-vars (arg-list)
  `(loop for var in ,arg-list append
	(let* ((len (length var))
	       (var-name (first var)))
	  (unless (symbolp var-name)
	    (error "invalid name for a variable: ~a" var-name))
	  (when (> len 10)
	    (warn "are you sure about ~a different instances of ~a?"
		  len var-name))
	  (when (> len 1)
	    (loop for i from 1 and var-def in (cdr var)
		 ,@(dynamic-collect (if (and (> (length (string var-name)) 3)
					     (equal "TIME" (subseq (string var-name) 0 4)))
					'with 'for)
				    (name-var var-name i)
				    '=
				    var-def))))))

;; ** fplay-test-for-var-name
#+nil(defmacro fplay-test-for-var-name (ls name &optional (init-form 1))
  `(if (and (> (length (string ,name)) 3)
	    (equal "TIME" (subseq (string ,name) 0 4)))
       (list 'with ,name '= ,init-form)
       (unless (assoc ,name ,ls)
	 (list 'for ,name '= ,init-form))))

;; ** merge-var-lists
(defmacro merge-var-lists (from into)
  `(loop for el in ,from unless (assoc (first el) ,into)
      do (push el ,into)))

;; ** fplay-get-loop-vars
;;; uses #'get-loop-vars and then checks, wheter the variable names that are
;;; needed for fplay are already assigned. If not, it does so here.
(defmacro fplay-get-loop-vars (start-time end-time arg-list)
  `(let* ((max-len (1- (apply #'max (mapcar #'length ,arg-list))))
	  (rthm (assoc 'rhythm ,arg-list))
	  (tim (assoc 'time ,arg-list))
	  (essential-names '())
	  (all-vars '()))
     ;; add as many time, break and line variables as needed
     (merge-var-lists
      (list (loop for i from 0 below (max 2 (length rthm) (length tim)) collect
		 (if (= 0 i) 'time
		     (if tim (nth (min (1- i) (length tim)) (cdr tim)) ,start-time)))
	    (loop for i from 0 below (max 2 (length rthm) (length tim)) collect
		 (if (= 0 i) 'break `(<= ,(name-var 'time i) ,,end-time)))
	    (loop for i from 0 below (max 2 (length rthm) (length tim)) collect
		 (if (= 0 i) 'line `(/ (- ,(name-var 'time i) ,,start-time)
				       (- ,,end-time ,,start-time)))))
      all-vars)
     ;;get user-defined variables in the mix
     (merge-var-lists ,arg-list all-vars)
     ;; add all other variables that are not set through arg-list
     (setf essential-names
	   (append '((sfl *percussive*) (sound (first (ly::data sfl)))
		     (file (ly::path sound)) (rhythm 1) (duration nil)
		     (reflect nil) (reverse nil) (start 0) (end 0) (srt 1)
		     (width 5) (srt-env '(0 0 100 0)) (srt-scaler 1.0) (amp 1.0)
		     (amp-env '(0 1 100 1)) (degree 45) (distance 0)
		     (rev-env '(0 1 100 1)) (rev-amt 0) (printing nil))
		   essential-names))
     ;; merge all variables into one list:
     (merge-var-lists essential-names all-vars)
     ;(print all-vars)
     ;; get all neccessary variables:
     (append (get-loop-vars (reverse all-vars))
	     ;; while:
	     (append '(while)
		     (list (loop for i from 0 below (max 2 (length rthm)) collect
				(if (= 0 i) 'or (name-var 'break i)))))
	     ;; instrument calls:
	     (loop for i from 1 to max-len append
		  (list 'when (name-var 'break 1) 'collect
			`(funcall (lambda ()
				    (samp1 ,(name-var-highest 'file i all-vars)
					   ,(name-var-highest 'time i all-vars)
					   :duration ,(name-var-highest 'duration i all-vars)
					   :reflect ,(name-var-highest 'reflect i all-vars)
					   :reverse ,(name-var-highest 'reverse i all-vars)
					   :start ,(name-var-highest 'start i all-vars)
					   :end ,(name-var-highest 'end i all-vars)
					   :srt ,(name-var-highest 'srt i all-vars)
					   :width ,(name-var-highest 'width i all-vars)
					   :srt-env ,(name-var-highest 'srt-env i all-vars)
					   :srt-scaler ,(name-var-highest 'srt-scaler i all-vars)
					   :amp ,(name-var-highest 'amp i all-vars)
					   :amp-env ,(name-var-highest 'amp-env i all-vars)
					   :degree ,(name-var-highest 'degree i all-vars)
					   :distance ,(name-var-highest 'distance i all-vars)
					   :rev-env ,(name-var-highest 'rev-env i all-vars)
					   :rev-amt ,(name-var-highest 'rev-amt i all-vars)
					   :printing ,(name-var-highest 'printing i all-vars)
					   )))))
	     ;; printing:
	     `(do ;(format t "~&time: ~a" time) (format t "~&rhythm: ~a" rhythm)
	       (format t "~&sound: ~a" (ly::id sound))
	       ,@(loop for i from 1 to (1- (length (assoc 'time all-vars))) collect
		      `(incf ,(name-var 'time i) ,(name-var-highest 'rhythm i all-vars)))
	       )))) ;,(name-var-highest 'rhythm i all-vars))))))

;; ** fplay
;;; macro to call samp1 instrument
(defmacro fplay (start-time end-time &rest rest)
  `(loop for i from 0 ,@(fplay-get-loop-vars start-time end-time rest)))

;; EOF utilities.lsp
