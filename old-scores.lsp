;; ** SCORE - number 1
;;; autoc - pitch estimation (Bret Battey) - autoc.ins ???
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/test.wav"
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
	  (interpolate-patterns (procession (1+ (length sec2-ly3-durations))
					    `(,*pattern5* ,*pattern6* ,*pattern7* ,*pattern7*))
				dur2 nil sec2-ly3-durations))
	 (rhythm2
	  (interpolate-patterns (fibonacci-transitions (1+ (length sec2-ly3-durations))
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
      (fb-play (startn 0) (startn 1)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (+ 0.5 (* line 0.5))
				    :degree (random 90))))
	       :rhythm-fun (- 30 (* (expt line 3) 25))
	       :duration (- 80 (* time 0.7))
	       :sound (nth (round (+ (* (sin (* (expt line 0.3) 5)) 0.5) 0.5))
			   (ly::data *pure_quiet*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 2
;;; TODO: make x y z dependend on time in rhythm
    (with-mix () "/E/code/feedback/build-pure" 0
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
    (with-mix () "/E/code/feedback/build-rhythm" 0
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :duration duration
				    :degree degree
				    :amp (* (- 0.99 (* line 0.5)) 0.6)))
		  #'(lambda () (samp1 file time
				      :duration duration
				      :degree degree
				      :srt 2
				      :amp (* line 0.55))))
	       :rhythm-list rhythm2
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :amp 0.6
				    :duration duration
				    :degree 0)))
	       :rhythm-list rhythm1
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :amp 0.6
				    :duration duration
				    :degree 90)))
	       :rhythm-list rhythm1
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))

;; ** SCORE - fb-play


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
    #+nil(with-mix () "/E/code/feedback/continuo" 0
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
					;:duration (/ rhythm (+ 1 (expt line 2)))
				    :amp 0.9
				    :amp-env *amp-env01*
				    :srt (if (< (rthms1 i 1) 0.3)
					     1 (srt-fun1 i))
				    :degree 0
				    :printing nil)))
	       :rhythm-fun (rthms1 i 0)	;(rthm2pat0 i 0) ;
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
	       :rhythm-fun (rthms1 i 1)	; (rthm2pat0 i 1) ;
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
	       :rhythm-fun (rthms1 i 2)	; (rthm2pat0 i 2) 
	       :sound (if (< (rthms1 i 2) 0.3)
			  (first (ly::data *percussive*))
			  (nth (sound-fun2 i)
			       (reverse (ly::data *quiet-atoms*))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #+nil(with-mix () "/E/code/feedback/beat" 0
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


#|
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
|#

;; ** SCORE - abandoning beats

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/test3.wav"
			  :channels 2 :play nil :scaled-to 0.95)
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
    (declare (special rhythm1 rhythm2 rhythm3 startn2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 1, Intro
    (with-mix () "/E/code/feedback/intro" 0
      (let* ((sound-list (reverse (ly::data *quiet-atoms*))))
        (fplay (startn 0) startn2
	       (srt (srt-fun1 i))
	       (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	       (sound (nth (mod (sound-fun2 i) 18) sound-list)
		      (nth (mod (sound-fun1 i) 18) sound-list))
	       (stop-in (- startn2 time) (- startn2 time2))
	       (duration (min (/ (ly::duration sound) srt) 5 stop-in)
			 (min (/ (ly::duration sound2) srt) stop-in2))
	       (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	       (amp-fade (if (> time (startn 1))
			     (expt (/ (- time (startn 1))
					   (- startn2 (startn 1)))
					2)
			     1))
	       (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) amp-fade)
		    (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) amp-fade))
	       (mult (+ 1 (* (expt line 0.3) 2)))
	       (rhythm (+ duration
			  (min (* (rest-fun2 i) mult) 5))
		       (+ duration2
			  (min (* (rest-fun2 i) mult) 5)))
	       (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;; Building on Intro
;;; new srt-funs
    (with-mix () "/E/code/feedback/continuo" 0
      (fplay (+ (startn 1) 5) startn2
	     (sounds (reverse (ly::data *quiet-atoms*)))
	     (last last-sound last-sound2 last-sound3) ; what if called on i = 0
	     ;;(new-sound (nth (sound-fun2 i) sounds))
	     (add (if (>= time (+ (startn 1) 15))
		      (* (- 1 (expt line 0.3)) 5) 0))
	     (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
		     (+ add (rthms1 i 2)))
	     (test (< rhythm 0.35) (< rhythm2 0.35) (< rhythm3 0.35))
	     (sound (if test last (nth (sound-fun2 i) sounds))
		    (if test2 last (nth (sound-fun2 i) sounds))
		    (if test3 last (nth (sound-fun2 i) sounds)))
	     (amp (if test (/ 0.2 (ly::peak sound)) 0.9)
		  (if test2 (/ .2 (ly::peak sound2)) 0.9)
		  (if test3 (/ .2 (ly::peak sound3)) 0.9))
	     (amp-env (if test '(0 0  5 1  80 0.8  100 0) *amp-env01*)
		      (if test2 '(0 0  5 1  80 0.8  100 0) *amp-env01*)
		      (if test3 '(0 0  5 1  80 0.8  100 0) *amp-env01*))
	     (counter (if test (+ counter .05) 0)
		      (if test2 (+ counter2 .05) 0)
		      (if test3 (+ counter3 .05) 0))
	     (start (if test (+ (* (ly::duration sound)
				   (/ (ly::peak-index sound)
				      (ly::total-samples sound)))
				counter)
			0)
		    (if test2 (+ (* (ly::duration sound2)
				    (/ (ly::peak-index sound2)
				       (ly::total-samples sound2)))
				 counter2)
			0)
		    (if test3 (+ (* (ly::duration sound3)
				    (/ (ly::peak-index sound3)
				       (ly::total-samples sound3)))
				 counter3)
			0))
	     (last-sound sound sound2 sound3)
	     (srt (if test 1 (srt-fun1 i))
		  (if test2 1 (srt-fun1 i))
		  (if test3 1 (srt-fun1 i)))
	     (stop-in (- startn2 time 2)
		      (- startn2 time2 2)
		      (- startn2 time3 2))
	     (duration (min (if test (* rhythm 0.8)
				(/ (ly::duration sound) srt))
			    stop-in)
		       (min (if test2 (* rhythm2 0.8)
				(/ (ly::duration sound2) srt2))
			    stop-in2)
		       (min (if test3 (* rhythm3 0.8)
				(/ (ly::duration sound3) srt3))
			    stop-in3))
	     (degree 0 (+ 45 (* (- (mod i 2) .5) 90 (- 1 line2))) 90)
	     (printing t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-mix () "/E/code/feedback/continuo2" 0
      (fplay (+ (startn 1) 5) (startn 2)
	     (sounds (reverse (ly::data *quiet-atoms*)))
	     (add (if (>= time (+ (startn 1) 15))
		      (* (- 1 (expt line 0.3)) 5) 0))
	     (srt (srt-fun2 i))
	     (rhythm (+ add (rthms1 i 0) (* (- srt 0.75) 1/3)))
	     (sound (nth (if (>= time 173) (sound-fun2 i) (sound-fun1 i))
			 sounds))
	     (duration (min (/ (ly::duration sound) srt)
			    (- (startn 2) time)))
	     (amp (if (> line .42) 0.9 0))
	     (amp-env *amp-env01*)
	     (degree 45)
	     (printing t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-mix () "/E/code/feedback/bass" 0
      (let ((file "/E/Keks_Feedback/samples/distorted/cookies_distorted_03.wav")
	    (file2 "/E/Keks_Feedback/samples/distorted/cookies_distorted_01a.wav")
	    (time (* 0.792 (startn 2)))
	    (time2 (* 0.8059 (startn 2)))
	    (srt 0.023181288)
	    (srt2 0.0487395))
	(samp1 file time :amp 1.3 :amp-env *amp-env01* :srt srt :degree 45
	       :duration (min (/ (ly::soundfile-duration file) srt)
			      (- startn2 time)))
	(samp1 file2 time2 :amp-env *amp-env01* :srt srt2 :degree 45
	       :duration (min (/ (ly::soundfile-duration file2) srt2)
			      (- startn2 time2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #+nil(with-mix () "/E/code/feedback/beat" 0
      (fplay (startn 1) (startn 2)
	     (amp 0.7)
	     (amp-env '(0 0  5 1  90 1  100 0))
	     (srt 10)
	     (sound (nth (spattern1 i) (reverse (ly::data *pure-atoms*))))
	     (rhythm (pattern3 i) (pattern4 i))
	     (duration (/ rhythm 3))
	     (degree 30 60)
	     (printig t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 3
    (with-mix () "/E/code/feedback/break" 0
      (fplay startn2 (startn 3)
	     (amp 0.5)
	     (sound (nth 0 (reverse (ly::data *noise*)))
		    (nth 1 (reverse (ly::data *noise*)))
		    (nth 2 (reverse (ly::data *noise*)))
		    (nth 3 (reverse (ly::data *noise*))))
	     (rhythm (br-rthms1 i)
		     (br-rthms2 i)
		     (br-rthms3 i)
		     (br-rthms4 i))
	     (duration (rationalize (* rhythm (- 1 (expt line 2))))
		       (rationalize (* rhythm2 (- 1 (expt line2 2))))
		       (rationalize (* rhythm3 (- 1 (expt line3 2))))
		       (rationalize (* rhythm4 (- 1 (expt line4 2)))))
	     (start "0 then (+ start duration)"
		    "0 then (ly::mirrors (+ start duration2) 0 4)"
		    "0 then (ly::mirrors (+ start duration3) 0 2.5)"
		    "0 then (ly::mirrors (+ start duration4) 0 4.5)")
	     (amp-env *amp-env01*)
	     #+nil(amp-env (if (< rhythm .2)
			  '(0 0  5 1  90 1  100 0)
			  '(0 0  99 1  100 0)))
	     (degl (* 90 line 2) (* 90 line2 2) (* 90 line3 2) (* 90 line4 2))
	     (degree (ly::mirrors (+ 0 degl) 0 90) (ly::mirrors (- 30 degl2) 0 90)
		     (ly::mirrors (- 60 degl3) 0 90) (ly::mirrors (+ 90 degl4) 0 90))
	     (printing nil)))
    #+nil(with-mix () "/E/code/feedback/beat2" 0
      (fplay (- (startn 2) 1.95) (startn 3)
	     (amp 0.7)
	     (amp-env '(0 0  5 1  90 1  100 0))
	     (srt 10)
	     (sound (nth (spattern1 i) (reverse (ly::data *noise*))))
	     (rhythm (* (pattern3 i) 0.5) (* (pattern4 i) .5))
	     (duration rhythm);(/ rhythm 3))
	     (degree 30 60)
	     (printig t)))
    #+nil(with-mix () "/E/code/feedback/beat3" 0
      (fplay (- (startn 2) 1.95) (startn 3)
	     (amp 0.7)
	     (amp-env '(0 0  5 1  90 1  100 0))
	     (srt 10)
	     (sound (nth (spattern1 i) (reverse (ly::data *noise*))))
	     (rhythm (* (pattern3 i) 0.125) (* (pattern4 i) .125))
	     (duration rhythm);(/ rhythm 3))
	     (degree 90 0)
	     (printig t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))
