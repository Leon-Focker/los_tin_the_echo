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
						'((1 1.25)
						  (2 1.5)
						  (3 0.2)
						  (4 1.3)
						  (5 1)
						  (6 0.2)
						  (7 0.95))
						:id 'form
						:type 'compartmentalise
						:duration (* 60 7.5)
						:smallest 2))
			    (form (cdr (reverse (ly::data st)))))
(ly::visualize-structure st 1 "/E/code/feedback/structure_comp1.png")
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

;;; some custom startns
(defparameter startn2 (- (startn 2) 1.95))

;; ** material

(defun wt (length levels &optional (e 0.8))
  (window-transitions length levels e 1))

;; collection of patterns, not very tidy
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
		       (loop repeat (+ (expt 2.1 i) 3) collect .2)))))))
       (pattern0 '(.3 .3 .3 .3 .3))
       (rthm2pat01 '())
       (rthm2pat02 '())
       (rthm2pat03 '())
       (ls '())
       (ls1 '()))
  (setf rthm3 (get-durations (append breaks rthm3)))
  (setf rthm4 (get-durations (append breaks rthm4)))
  (setf rthm5 (get-durations (append breaks rthm5)))
  (setf rthm2pat01 (morph-patterns (list rthm3 pattern0) len nil t nil
				   (fibonacci-transition 40)))
  (setf rthm2pat02 (morph-patterns (list rthm4 pattern0) len nil t nil
				   (fibonacci-transition 70)))
  (setf rthm2pat03 (morph-patterns (list rthm5 pattern0) len nil t nil
				   (fibonacci-transition 100)))
  (setf ls (list rthm3 rthm4 rthm5))
  (setf ls1 (list rthm2pat01 rthm2pat02 rthm2pat03))
  (defun rthms1 (i rthm-index)
    (let ((l (nth rthm-index ls)))
      (nth (mod i (length l)) l)))
  (defun rthm2pat0 (i rthm-index)
    (let ((l (nth rthm-index ls1)))
    (nth (mod i (length l)) l))))

;; some rhythm-based patterns
(let* ((pattern1 '(.2 .2 .2 .2 .2))
       (spattern1 '(0 0 0 0 5))
       (pattern2 '(.3 .3 .3 .1))
       ;;(spattern2 '(0 0 0 5))
       (pattern3 (morph-patterns (list pattern1 pattern2) 60 nil t nil
				 (fibonacci-transition 120)))
       (pattern4 (morph-patterns (list pattern1 pattern2) 60 nil t nil
				 (fibonacci-transition 80)))
       (len1 (length pattern1))
       ;;(len2 (length pattern2))
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

;; Some patterns, primarily for intro
(let* ((len 30)
       (quiet-len (length (ly::data *quiet-atoms*)))
       (ls1 (avoid-repetition
	     (procession len (loop for i below quiet-len collect i))))
       (len1 (length ls1))
       (ls2 (avoid-repetition (wt len quiet-len)))
       (len2 (length ls2))
       (ls3 (procession len '(0 3 0.8 5)))
       (ls4 (wt (+ len 5) '(0 2.5 0.8 5 1) 0.5))
       (ls5 (wt len '(0 0.05 .1 .2 .35 .4)))
       (ls6 (wt len '(0 0.08 .15 .23 .3 .45)))
       (ls7 (wt (* len 2) (length (ly::data *pure-atoms*)))))
  #+nil(loop for i in '(7 9 13) do
       (incf (nth i ls4) 20))
  (defun sound-fun1 (i)
    (nth (mod i len1) ls1))
  (defun sound-fun2 (i)
    (nth (mod i len2) ls2))
  (defun sound-fun3 (i)
    (nth (mod i (* 2 len)) ls7))
  (defun rest-fun1 (i)
    (nth (mod i len) ls3))
  (defun rest-fun2 (i)
    (nth (mod i len) ls4))
  (defun srt-fun1 (i)
    (+ 0.6 (nth (mod i len) ls5)))
  (defun srt-fun2 (i)
    (+ 0.6 (nth (mod i len) ls6))))

;; break rhythms
(let* ((pt1 '(.05))
       (pt2 '(.1))
       (pt3 '(1))
       (pt4 '(.5))
       ;;(pt5 '(.02))
       (dur (- (startn 3) (startn 2)))
       #|(ls1 (morph-patterns `(,pt3 ,pt1) dur nil t nil))
       (ls2 (morph-patterns `(,pt3 ,pt2) dur nil t nil))
       (ls3 (morph-patterns `(,pt4 ,pt2) dur nil t nil (wt 20 2)))
       (ls4 (morph-patterns `(,pt4 ,pt1) dur nil t nil (wt 20 2)))|#
       (ls1 (interpolate-patterns (fibonacci-transitions 6 `(,pt3 ,pt1)) dur t))
       (ls2 (interpolate-patterns (fibonacci-transitions 8 `(,pt3 ,pt2)) dur t))
       (ls3 (interpolate-patterns (wt 7 `(,pt4 ,pt2) 1) dur t))
       (ls4 (interpolate-patterns (wt 7 `(,pt4 ,pt1)) dur t))
       (len1 (length ls1))
       (len2 (length ls2))
       (len3 (length ls3))
       (len4 (length ls4)))
  (defun br-rthms1 (i)
    (nth (mod i len1) ls1))
  (defun br-rthms2 (i)
    (nth (mod i len2) ls2))
  (defun br-rthms3 (i)
    (nth (mod i len3) ls3))
  (defun br-rthms4 (i)
    (nth (mod i len4) ls4)))

;; ** Generation

;;; Transitions:
;;; autoc - pitch estimation (Bret Battey) - autoc.ins ???
;;; TODO: Multichannel
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
	     (add (if (>= time (+ (startn 1) 15))
		      (* (- 1 (expt line 0.3)) 5) 0))
	     (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
		     (+ add (rthms1 i 2)))
	     (sound (nth (sound-fun2 i) sounds)
		    (nth (sound-fun2 i) sounds)
		    (nth (sound-fun2 i) sounds))
	     (amp .9)
	     (amp-env *amp-env01*)
	     (srt (srt-fun1 i) (srt-fun1 i) (srt-fun1 i))
	     (stop-in (- startn2 time 2)
		      (- startn2 time2 2)
		      (- startn2 time3 2))
	     (duration (min (/ (ly::duration sound) srt)
			    stop-in)
		       (min (/ (ly::duration sound2) srt2)
			    stop-in2)
		       (min (/ (ly::duration sound3) srt3)
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
	     (amp 0.7)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 4
;;; Intro again for now :PP
    (with-mix () "/E/code/feedback/mid" 0
      (let* ((sound-list (reverse (ly::data *quiet-atoms*))))
        (fplay (startn 3) (startn 4)
	       (srt (srt-fun1 i))
	       (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	       (sound (nth (mod (sound-fun2 i) 18) sound-list)
		      (nth (mod (sound-fun1 i) 18) sound-list))
	       (stop-in (- (startn 4) time) (- (startn 4) time2))
	       (duration (min (/ (ly::duration sound) srt) 5 stop-in)
			 (min (/ (ly::duration sound2) srt) stop-in2))
	       (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	       (amp (dry-wet 0.9 amp-mult (* line 0.3))
		    (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)))
	       (mult (+ 1 (* (expt line 0.3) 2)))
	       (rhythm (+ duration
			  (min (* (rest-fun2 i) mult) 5))
		       (+ duration2
			  (min (* (rest-fun2 i) mult) 5)))
	       (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))

;; EOF score.lsp
