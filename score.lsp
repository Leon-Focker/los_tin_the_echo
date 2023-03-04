;; * SCORE

;; ** form
;; 1 intro (pure)
;; 2 rhythmen (pure + percussive)
;; 3 short noise block (noise)
;; 4 rhythmic noise blocks (dist + noise)
;; 5 distortion (dist)
;; 6 short outro (quiet dist)
(defparameter *form* (let* ((st (make-structure '(1 2 3 4 5 6)
						'((1 ((1)))
						  (2 ((4 3 2 5)))
						  (3 ((6)))
						  (4 ((7 4 5)))
						  (5 ((2 4 5 3)))
						  (6 ((1)))
						  (7 ((3))))
						'((1 1)
						  (2 1.5)
						  (3 0.2)
						  (4 1.3)
						  (5 1)
						  (6 0.2)
						  (7 0.95))
						:id 'form
						:type 'compartmentalise
						:duration (* 60 7)
						:smallest 2))
			    (form (cdr (reverse (ly::data st)))))
		       (ly::visualize-structure st 1 "/E/code/feedback/structure_comp1.png")
		       form))

;; ** material

(defun wt (length levels &optional (e 0.8))
  (window-transitions length levels e 1))

;; rhythms of type 1
(let* ((len 30)
       (rthm1 (get-start-times (wt len '(0.8 2 3.6 6.9 2.5))))
       (rthm2 (get-start-times (wt len '(0.8 2 3.6 6.9 2.5) 0.3)))
       (rthm3 (morph-patterns (list rthm1 rthm2) 100 nil t len (wt len 4 .3)))
       (rthm4 (morph-patterns (list rthm1 rthm2) 100 nil t len (wt len 5 .3)))
       (rthm5 (morph-patterns (list rthm1 rthm2) 100 nil t len (wt len 6 .3)))
       (ls '()))
  (setf rthm3 (get-durations (sort rthm3 #'<)))
  (setf rthm4 (get-durations (sort rthm4 #'<)))
  (setf rthm5 (get-durations (sort rthm5 #'<)))
  (setf ls (list rthm3 rthm4 rthm5))
  (defun rthms1 (i rthm-index)
    (nth (mod i len) (nth rthm-index ls))))

;; collection of some different lists
(let* ((len 30)
       (ls1 (avoid-repetition (procession len (loop for i below 18 collect i))))
       (len1 (length ls1))
       (ls2 (avoid-repetition (wt len 18)))
       (len2 (length ls2))
       (ls3 (procession len '(0 3 0.8 5)))
       (ls4 (wt (+ len 5) '(0 3 0.8 5 1) 0.5))
       (ls5 (wt len '(0 0.05 .1 .2 .35 .4))))
  (defmacro i-from (ls &optional (len len))
    `(nth (mod i ,len) ,ls))
  (defun sound-fun1 (i)
    (i-from ls1 len1))
  (defun sound-fun2 (i)
    (i-from ls2 len2))
  (defun rest-fun1 (i)
    (i-from ls3))
  (defun rest-fun2 (i)
    (i-from ls4))
  (defun srt-fun1 (i)
    (+ 0.6 (i-from ls5))))

;; ** Generation

;;; Transitions:
;;; autoc - pitch estimation (Bret Battey) - autoc.ins ???
;;; TODO: some way of avoiding loading samples again and again
;;; TODO: Multichannel
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/test3.wav"
			  :channels 2 :play nil)
;;; times and durations
  (let* ((start-times (get-start-times (first *form*)))
	 (sec2-ly3-durations
	  (get-durations-within (third *form*) (startn 1) (startn 2)))
	 #+nil(sec2-ly3-start-times
	       (get-start-times-within (third *form*) (startn 1) (startn 2)))
	 (dur2 (- (startn 2) (startn 1)))
;;; rhythms and patterns
	 (rhythm1
	  (interpolate-patterns
	   (procession (1+ (length sec2-ly3-durations))
		       `(,*pattern5* ,*pattern6* ,*pattern7* ,*pattern7*))
	   dur2 nil sec2-ly3-durations))
	 (rhythm2
	  (interpolate-patterns
	   (fibonacci-transitions (1+ (length sec2-ly3-durations))
				  `(,*pattern5* ,*pattern6* ,*pattern7*))
	   dur2 nil sec2-ly3-durations))
	 (rhythm3
	  (morph-patterns (list *pattern4* *pattern1*)
			  dur2 nil (fibonacci-transition dur2))))
    (declare (special start-times rhythm1 rhythm2 rhythm3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 1
;;; TODO: how many channels?
;;; TODO: strategies to avoid random?
    (with-mix () "/E/code/feedback/intro" 0
      (fb-play (startn 0) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (srt-fun1 i)
				    :degree (random 90)
				    :printing nil)))
	       ;; duration of the sample + rest
	       :rhythm-fun (+ (* (ly::duration sound) (/ 1 (srt-fun1 i)))
			      (rest-fun2 i))
	       ;;:duration (- 80 (* time 0.7))
	       :sound (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*))))
      (fb-play (startn 0) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp (* line 0.7)
				    :amp-env *amp-env01*
				    :srt (srt-fun1 i)
				    :degree 45
				    :printing nil)))
	       ;; duration of the sample + rest
	       :rhythm-fun (+ (* (ly::duration (nth (sound-fun2 i)
						    (reverse (ly::data *quiet-atoms*))))
				 (/ 1 (srt-fun1 i)))
			      (rest-fun2 i))
	       :sound (nth (sound-fun1 i)
			   (reverse (ly::data *quiet-atoms*)))))
    (with-mix () "/E/code/feedback/continuo" 0
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (srt-fun1 i)
				    :degree 0
				    :printing nil)))
	       ;; duration of the sample + rest
	       :rhythm-fun (rthms1 i 1)
	       ;;:duration (- 80 (* time 0.7))
	       :sound (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (srt-fun1 i)
				    :degree 45
				    :printing nil)))
	       ;; duration of the sample + rest
	       :rhythm-fun (rthms1 i 2)
	       ;;:duration (- 80 (* time 0.7))
	       :sound (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (srt-fun1 i)
				    :degree 90
				    :printing nil)))
	       ;; duration of the sample + rest
	       :rhythm-fun (rthms1 i 3)
	       ;;:duration (- 80 (* time 0.7))
	       :sound (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 2
;;; TODO: make x y z dependend on time in rhythm
    #+nil(with-mix () "/E/code/feedback/build-pure" 0
	   (fb-play (startn 1) (startn 2)
		    (#'(lambda() (samp1 file time
					:duration duration
		  			:amp 0.5
					:amp-env *amp-env01*
					:srt 1
					:degree (random 90))))
		    :rhythm-fun (+ 5 (* (expt line 3) 25))
		    :duration (- (startn 2)  time)
		    :sound (nth (round (+ (* (sin (* (expt line 0.3) 5)) 0.5) 0.5))
				(ly::data *pure*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))

;; EOF score.lsp
