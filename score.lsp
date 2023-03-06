;; * SCORE

(in-package :fb)

;; TODO: floats and rationals

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
		       ;;(ly::visualize-structure st 1 "/E/code/feedback/structure_comp1.png")
		       form))

(defparameter *sections* (first *form*))
(defparameter *start-times-1* (first *form*))

(defun start-times (n-for-layer)
  (when (> n-for-layer (1- (length *form*)))
    (error "n-for-layer is too big for *form*: ~&~a" n-for-layer))
  (get-start-times (nth n-for-layer *form*)))

;;; return the start-times of the nth section (starting with 0)
(defun startn (index)
  (nth index (start-times 0)))

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
       (breaks0 (avoid-repetition (wt 30 '(15 10 1 12 1))))
       (indices (cdr (loop for i from 0 and k = (length breaks0) then (- k i)
		     until (<= k 0) collect k)))
       (breaks (get-start-times
		(flatten
		 (insert-multiple 
		  (scale-list-to-sum breaks0
				     (- (second (first *form*)) 10))
		  indices
		  (reverse (loop for i from 0 below (length indices) collect
		       (loop repeat (expt 2.1 i) collect .2)))))))
       (pattern0 '(.2 .2 .2 .2 .2))
       (rthm2pat01 '())
       (rthm2pat02 '())
       (rthm2pat03 '())
       (ls '())
       (ls1 '()))
  (setf rthm3 (get-durations (append breaks rthm3)))
  (setf rthm4 (get-durations (append breaks rthm4)))
  (setf rthm5 (get-durations (append breaks rthm5)))
  ;(setf rthm2pat01 (morph-patterns (list rthm3 pattern0) len nil t nil (wt 1000 25 20)))
  ;(setf rthm2pat02 (morph-patterns (list rthm4 pattern0) len nil t nil (wt 1000 30 20)))
  ;(setf rthm2pat03 (morph-patterns (list rthm5 pattern0) len nil t nil (wt 800 25 21)))
  (setf rthm2pat01 (morph-patterns (list rthm3 pattern0) len nil t nil (fibonacci-transition 40)))
  (setf rthm2pat02 (morph-patterns (list rthm4 pattern0) len nil t nil (fibonacci-transition 70)))
  (setf rthm2pat03 (morph-patterns (list rthm5 pattern0) len nil t nil (fibonacci-transition 100)))
  (setf ls (list rthm3 rthm4 rthm5))
  (setf ls1 (list rthm2pat01 rthm2pat02 rthm2pat03))
  (defun rthms1 (i rthm-index)
    (let ((l (nth rthm-index ls)))
      (nth (mod i (length l)) l)))
  (defun rthm2pat0 (i rthm-index)
    (let ((l (nth rthm-index ls1)))
    (nth (mod i (length l)) l))))

(let* ((pattern1 '(.2 .2 .2 .2 .2))
       (spattern1 '(0 0 0 0 5))
       (pattern2 '(.3 .3 .3 .1))
       (spattern2 '(0 0 0 5))
       (pattern3 (morph-patterns (list pattern1 pattern2) 60 nil t nil
				 (fibonacci-transition 120)))
       (pattern4 (morph-patterns (list pattern1 pattern2) 60 nil t nil
				 (fibonacci-transition 80)))
       (len1 (length pattern1))
       (len2 (length pattern2))
       (len3 (length pattern3))
       (len4 (length pattern4)))
  (defun pattern1 (i)
    (nth (mod i len1) pattern1))
  (defun spattern1 (i)
    (nth (mod i len1) spattern1))
  (defun pattern3 (i)
    (nth (mod i len3) pattern3))
  (defun pattern4 (i)
    (nth (mod i len4) pattern4)))

;; collection of some different lists
(let* ((len 30)
       (quiet-len (length (ly::data *pure-atoms*)))
       (ls1 (avoid-repetition
	     (procession len (loop for i below quiet-len collect i))))
       (len1 (length ls1))
       (ls2 (avoid-repetition (wt len quiet-len)))
       (len2 (length ls2))
       (ls3 (procession len '(0 3 0.8 5)))
       (ls4 (wt (+ len 5) '(0 3 0.8 5 1) 0.5))
       (ls5 (wt len '(0 0.05 .1 .2 .35 .4)))
       (ls6 (wt (* len 2) (length (ly::data *pure-atoms*)))))
  (defun sound-fun1 (i)
    (nth (mod i len1) ls1))
  (defun sound-fun2 (i)
    (nth (mod i len2) ls2))
  (defun sound-fun3 (i)
    (nth (mod i (* 2 len)) ls6))
  (defun rest-fun1 (i)
    (nth (mod i len) ls3))
  (defun rest-fun2 (i)
    (nth (mod i len) ls4))
  (defun srt-fun1 (i)
    (+ 0.6 (nth (mod i len) ls5))))

;; ** Generation

;;; Transitions:
;;; autoc - pitch estimation (Bret Battey) - autoc.ins ???
;;; TODO: some way of avoiding loading samples again and again
;;; TODO: Multichannel
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/test3.wav"
			  :channels 2 :play nil)
;;; times and durations
  (let* ((sec2-ly3-durations
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
    (declare (special rhythm1 rhythm2 rhythm3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 1
;;; TODO: how many channels?
;;; TODO: strategies to avoid random?
    (with-mix () "/E/code/feedback//intro" 0
      (play start end
	    (sound (first (ly:data *quiet-atoms*)) (nth i (ly:data *quiet-atoms*)))
	    (rhythm (rthms1 i 0) 1))

      (loop for i from 0
	 with time = start while (<= time end)
	 for line = (/ (- time start) (- end start))
	 for sound1 = (first (ly:data *quiet-atoms*))
	 for sound2 = (nth i (ly:data *quiet-atoms*))
	 for file1 = (ly::path sound1)
	 for file2 = (ly::path sound2)
	 for rhythm1 = (rthms1 i 0)
	 for rhythm2 = 1
	 for duration = nil
	 collect (samp1 file1 time)
	 collect (samp1 file2 time)
	 do (incf time rhythm1))

      (loop for i from 0
	 with time1 = start
	 with time2 = start
	 for break1 = (<= time1 end)
	 for break2 = (<= time2 end)
	 while (or break1 break2)
	 for line = (/ (- time start) (- end start))
	 for sound1 = (first (ly:data *quiet-atoms*))
	 for sound2 = (nth i (ly:data *quiet-atoms*))
	 for file1 = (ly::path sound1)
	 for file2 = (ly::path sound2)
	 for rhythm1 = (rthms1 i 0)
	 for rhythm2 = 1
	 for duration = nil
	 when break1 collect (samp1 file1 time :duration duration...)
	 when break2 collect (samp1 file2 time :duration duration...)
	 do (incf time1 rhythm1)
	   (incf time2 rhythm2))
      )
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
						    (reverse (ly::data
							      *quiet-atoms*))))
				 (/ 1 (srt-fun1 i)))
			      (rest-fun2 i))
	       :sound (nth (sound-fun1 i)
			   (reverse (ly::data *quiet-atoms*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 2
    (with-mix () "/E/code/feedback/continuo" 0
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    ;:duration (/ rhythm (+ 1 (expt line 2)))
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (if (< (rthms1 i 1) 0.3)
					     1 (srt-fun1 i))
				    :degree 0
				    :printing nil)))
	       :rhythm-fun (rthms1 i 0);(rthm2pat0 i 0) ;
	       :sound (if (< (rthms1 i 0) 0.3)
			  (first (ly::data *percussive*))
			  (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*)))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				   ; :duration (/ rhythm (+ 1 (expt line 2)))
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (if (< (rthms1 i 1) 0.3)
					     1 (srt-fun1 i))
				    :degree 45
				    :printing nil)))
	       :rhythm-fun (rthms1 i 1); (rthm2pat0 i 1) ;
	       :sound (if (< (rthms1 i 1) 0.3)
			  (first (ly::data *percussive*))
			  (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*)))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				   ; :duration (/ rhythm (+ 1 (expt line 2)))
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (if (< (rthms1 i 1) 0.3)
					     1 (srt-fun1 i))
				    :degree 90
				    :printing nil)))
	       :rhythm-fun (rthms1 i 2); (rthm2pat0 i 2) 
	       :sound (if (< (rthms1 i 2) 0.3)
			  (first (ly::data *percussive*))
			  (nth (sound-fun2 i)
			   (reverse (ly::data *quiet-atoms*))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-mix () "/E/code/feedback/beat" 0
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration (/ rhythm 3)
				    :amp 0.7
				    :amp-env *amp-env01*
				    :srt 1
				    :degree 30
				    :printing nil)))
	       :rhythm-fun (pattern3 i)
	       :sound (nth (spattern1 i)
			   (reverse (ly::data *percussive*))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration (/ rhythm 3)
				    :amp 0.7
				    :amp-env *amp-env01*
				    :srt 1
				    :degree 60
				    :printing nil)))
	       :rhythm-fun (pattern4 i)
	       :sound (nth (spattern1 i)
			   (reverse (ly::data *percussive*)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/fplay-test.wav"
			  :channels 2 :play nil)
  (fb-play 0 10 (#'(lambda () (samp1 file time))) :rhythm-list '(1 1)))

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/fplay-test.wav"
			  :channels 2 :play nil)
  (fplay 0 10 (file "/E/Keks_Feedback/samples/percussive/polished/cookies_percussive_32.wav" "/E/Keks_Feedback/samples/percussive/polished/cookies_percussive_30.wav")
	 (time 0 1)
	 (rhythm (nth i '(1 2 1 2 1 2 1 2 1 2)) (nth i '(2 1 2 1 2 1 2 1 2 1 2)))))

;; EOF score.lsp
