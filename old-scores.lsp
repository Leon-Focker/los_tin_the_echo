;; ** SCORE
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
