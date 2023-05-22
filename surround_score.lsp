;; * SCORE
;;; Generation of the piece happens here, check out the documentation for each
;;; function to find out how things work :]

(in-package :fb)

(format t "~&processing SCORE")

;; TODO: *distorted* contains raw samples (bc folder inside foldwer) :/

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

;;; some custom startns
(defparameter startn2 (- (startn 2) 1.95))
(defparameter startn3 (+ (startn 3) 21))
(defparameter startn4 (+ (startn 4) 27))

;; ** mixer

(defun fader (env &optional (line 0))
  (if (numberp env)
      env
      (envelope-interp line env)))

(defparameter *intro-f* 1)
(defparameter *intro-n-f* 0.55)
(defparameter *continuo-f* 0.8)
(defparameter *continuo2-f* 0.8)
(defparameter *bass-f* 0.8)
(defparameter *break-f* 0.9)
(defparameter *break02-f* 0.9)
(defparameter *rhythms-f* 0.8)
(defparameter *bridge-f* 0.6)
(defparameter *mid-f* 0.6)
(defparameter *mid-dist-f* .6)
(defparameter *remix-f* .7)
(defparameter *disruptor-f* 1)
(defparameter *last-f* 1)
(defparameter *bass-last-f* .85)
(defparameter *outro-f* 1)

;; prevent style-warnings:
;;(defun :scaled-by ())

;; ** material

(defun wt (length levels &optional (e 0.8))
  (window-transitions length levels e 1))

;; collection of patterns, not very tidy
(let* ((len 30)
       (rthm1 (get-start-times (wt len '(0.8 2 3.6 6.9 2.5))))
       (rthm2 (get-start-times (wt len '(0.8 2 3.6 6.9 2.5) 0.3)))
       (rthm3 (morph-patterns (list rthm1 rthm2) 100 nil t len nil (wt len 4 .3)))
       (rthm4 (morph-patterns (list rthm1 rthm2) 100 nil t len nil (wt len 5 .3)))
       (rthm5 (morph-patterns (list rthm1 rthm2) 100 nil t len nil (wt len 6 .3)))
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
  (setf rthm2pat01 (morph-patterns (list rthm3 pattern0) len nil t nil nil
				   (fibonacci-transition 40)))
  (setf rthm2pat02 (morph-patterns (list rthm4 pattern0) len nil t nil nil
				   (fibonacci-transition 70)))
  (setf rthm2pat03 (morph-patterns (list rthm5 pattern0) len nil t nil nil
				   (fibonacci-transition 100)))
  (setf ls (list rthm3 rthm4 rthm5))
  (setf ls1 (list rthm2pat01 rthm2pat02 rthm2pat03))
  (defun rthms1 (i rthm-index)
    (let ((l (nth rthm-index ls)))
      (nth (mod i (length l)) l)))
  (defun rthm2pat0 (i rthm-index)
    (let ((l (nth rthm-index ls1)))
    (nth (mod i (length l)) l))))

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
       (ls-noise (interpolate-patterns `(,ls4 ,ls3) (* len 3) t ))
       (len-noise (length ls-noise))
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
  (defun rest-fun2-noise (i)
    (nth (mod i len-noise) ls-noise))
  (defun srt-fun1 (i)
    (+ 0.6 (nth (mod i len) ls5)))
  (defun srt-fun2 (i)
    (+ 0.6 (nth (mod i len) ls6))))

;; break rhythms
(let* ((pt1 '(.05 .03))
       (pt2 '(.1 .023))
       (pt3 '(1 .7))
       (pt4 '(.5 .1))
       (pt5 '(.02 .01))
       (pt6 '(.09))
       (pt7 '(.145))
       (dur (- (startn 3) (startn 2)))
       (dur2 (- startn3 (startn 2)))
       #|(ls1 (morph-patterns `(,pt3 ,pt1) dur nil t nil))
       (ls2 (morph-patterns `(,pt3 ,pt2) dur nil t nil))
       (ls3 (morph-patterns `(,pt4 ,pt2) dur nil t nil nil (wt 20 2)))
       (ls4 (morph-patterns `(,pt4 ,pt1) dur nil t nil nil (wt 20 2)))|#
       (ls1 (interpolate-patterns (fibonacci-transitions 6 `(,pt3 ,pt1)) dur t))
       (ls2 (interpolate-patterns (fibonacci-transitions 8 `(,pt3 ,pt2)) dur t))
       (ls3 (interpolate-patterns (wt 7 `(,pt4 ,pt2) 1) dur t))
       (ls4 (interpolate-patterns (wt 7 `(,pt4 ,pt1)) dur t))
       (ls5 (interpolate-patterns `(,pt5 ,pt6 ,pt5 ,pt7 ,pt5 ,pt6 ,pt7) dur2 t '(1 2 3 2 5 6)))
       (ls6 (interpolate-patterns `(,pt5 ,pt6 ,pt7 ,pt6 ,pt5 ,pt7) dur2 t '(1 2 3 2 5)))
       (ls7 (morph-patterns `(,ls5 ,ls6) dur2 nil t nil nil (fibonacci-transition 8)))
       (ls8 (morph-patterns `(,ls5 ,ls6) dur2 nil t nil nil (fibonacci-transition 9)))
       (ls9 (morph-patterns `(,ls5 ,ls6) dur2 nil t nil nil (fibonacci-transition 10)))
       (ls10 (morph-patterns `(,ls5 ,ls6) dur2 nil t nil nil (fibonacci-transition 11)))
       (len1 (length ls1))
       (len2 (length ls2))
       (len3 (length ls3))
       (len4 (length ls4))
       (len5 (length ls5))
       (len6 (length ls6))
       (len7 (length ls7))
       (len8 (length ls8))
       (len9 (length ls9))
       (len10 (length ls10)))
  (defun br-rthms1 (i)
    (nth (mod i len1) ls1))
  (defun br-rthms2 (i)
    (nth (mod i len2) ls2))
  (defun br-rthms3 (i)
    (nth (mod i len3) ls3))
  (defun br-rthms4 (i)
    (nth (mod i len4) ls4))
  (defun br-rthms5 (i)
    (nth (mod i len5) ls5))
  (defun br-rthms6 (i)
    (nth (mod i len6) ls6))
  (defun br-rthms7 (i)
    (nth (mod i len7) (reverse ls7)))
  (defun br-rthms8 (i)
    (nth (mod i len8) (reverse ls8)))
  (defun br-rthms9 (i)
    (nth (mod i len9) ls9))
  (defun br-rthms10 (i)
    (nth (mod i len10) ls10))
  (defun br-rthms11 (i)
    (nth (mod i len10) (reverse ls10)))
  (defun br-rthms12 (i)
    (nth (mod i len9) (reverse ls9))))

;; ** Generation

;;; TODO: Multichannel
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/just_feed_it_surround.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 1, Intro
  #+nil(with-mix () "/E/code/feedback/intro_surround" 0
    (let* ((sound-list (reverse (ly::data *quiet-atoms*))))
		  (fplay (startn 0) startn2
			 (srt (srt-fun1 i))
			 (srt-env (if (<= 95 time 120)
				      '(0 0 100 0)
				      (srt-break (- 100 (* (expt line 2) 50))
						 (srt-fun1 (1- i))))
				  (if (<= 95 time2 120)
				      '(0 0 100 0)
				      (srt-break (- 100 (* (expt line2 2) 50))
						 (srt-fun1 (1- i)))))
			 (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
			 (sound (nth (mod (sound-fun1 i) 16) sound-list)
				(nth (mod (sound-fun2 i) 12) sound-list))
			 (stop-in (- startn2 time) (- startn2 time2))
			 (duration (min (/ (ly::duration sound) srt) 5 stop-in)
				   (min (/ (ly::duration sound2) srt) stop-in2))
			 (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
			 (amp-fade (if (> time (startn 1))
				       (expt (/ (- time (startn 1))
						(- startn2 (startn 1)))
					     2)
				       1))
			 (fader (fader *intro-f* line) (fader *intro-f* line2))
			 (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) amp-fade fader)
			      (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) amp-fade
				 fader2))
			 (mult (+ 1 (* (expt line 0.3) 2)))
			 (rhythm (+ duration
				    (min (* (rest-fun2 i) mult) 5))
				 (+ duration2
				    (min (* (rest-fun2 i) mult) 5)))
			 (degree 0 90)
			 (printing t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
  (with-mix () "/E/code/feedback/continuo_surround" 0
    (fplay (+ (startn 1) 5) startn2
	   (sounds (reverse (ly::data *quiet-atoms*)))
	   (add (if (>= time (+ (startn 1) 15))
		    (* (- 1 (expt line 0.3)) 5) 0))
	   (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
		   (+ add (rthms1 i 2)))
	   (sound (nth (sound-fun1 i) sounds)
		  (nth (sound-fun1 i) sounds)
		  (nth (sound-fun1 i) sounds))
	   (fader (fader *continuo-f* line) (fader *continuo-f* line2)
		  (fader *continuo-f* line3))
	   (amp (* .9 fader) (* .91 fader2) (* .91 fader3))
	   (amp-env *amp-env01*)
	   (srt (srt-fun1 i) (srt-fun1 i) (srt-fun1 i))
	   (srt-env (srt-break (- 90 (* (expt line 2) 30))
			       (- (/ (srt-fun1 (1+ i)) 2))
			       (+ (* (expt line 2) .5) .001))
		    (srt-break (- 90 (* (expt line2 2) 40))
			       (- (/ (srt-fun1 (1+ i)) 3))
			       (+ (* (expt line2 2) .5) .001))
		    (srt-break (- 90 (* (expt line3 2) 35))
			       (- (/ (srt-fun1 (1- i)) 2))
			       (+ (* (expt line3 2) .5) .001)))
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
  (with-mix () "/E/code/feedback/continuo2_surround" 0
    (fplay (+ (startn 1) 5) (startn 2)
	   (sounds (reverse (ly::data *quiet-atoms*)))
	   (add (if (>= time (+ (startn 1) 15))
		    (* (- 1 (expt line 0.3)) 5) 0))
	   (srt (srt-fun2 i))
	   (rhythm (+ add (rthms1 i 0) (* (- srt 0.75) 1/3)))
	   (sound (nth (if (>= time 173) (sound-fun1 i) (sound-fun2 i))
		       sounds))
	   (duration (min (/ (ly::duration sound) srt)
			  (- (startn 2) time)))
	   (fader (fader *continuo2-f* line))
	   (amp (* (if (> line .42) 0.9 0) fader))
	   (amp-env *amp-env01*)
	   (degree 45)
	   (printing t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rhythms during 4th part.
  (with-mix () "/E/code/feedback/rhythms_surround" 0
    (let* ((sound-list (reverse (ly::data *noise*))))
      (fplay startn3 (+ startn4 52.5)
	     (sound (nth 6 sound-list))
	     (rthm (* (br-rthms7 i) (- 1 (* (expt line 2) .4)))
		   (* (br-rthms8 i) (- 1 (* (expt line 2) .4)))
		   (* (br-rthms9 i) (- 1 (* (expt line 2) .4)))
		   (* (br-rthms10 i) (- 1 (* (expt line 2) .4))))
	     (rhythm rthm rthm3 rthm rthm rthm2 rthm2 rthm4 rthm2)
	     (srt (dry-wet .2 8 (expt line 2)))
	     (duration (rationalize (if (> line .3) (* rhythm .01)
					(* rhythm (dry-wet 0.01 .2 (- line .6))))))
	     (fader (fader *rhythms-f* line))
	     (amp (* (dry-wet .03 .15 (expt line 1.2)) fader))
	     (start "1 then (ly::mirrors (+ start duration) 1 4)"
		    "2 then (ly::mirrors (+ start duration) 1 4)"
		    "3 then (ly::mirrors (+ start duration) 1 4)"
		    "4 then (ly::mirrors (+ start duration) 1 4)"
		    "1 then (ly::mirrors (+ start duration) 1 4)"
		    "2 then (ly::mirrors (+ start duration) 1 4)"
		    "3 then (ly::mirrors (+ start duration) 1 4)"
		    "4 then (ly::mirrors (+ start duration) 1 4)")
	     (amp-env (env-fun1 10 2))
	     (degl (* 90 line 5) (* 90 line 5) (* 90 line 5) (* 90 line 5)
		   (* 90 line 8) (* 90 line 8) (* 90 line 8) (* 90 line 8))
	     (degree (ly::mirrors degl 0 90) (ly::mirrors (+ 30 degl2) 0 90)
		     (ly::mirrors (+ 60 degl3) 0 90)
		     (ly::mirrors (- 90 degl4) 0 90)
		     (ly::mirrors degl 0 90) (ly::mirrors (+ 30 degl2) 0 90)
		     (ly::mirrors (+ 60 degl3) 0 90)
		     (ly::mirrors (- 90 degl4) 0 90))
	     (printing nil nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 4
;;; Condinuo again, but not quiet
  (with-mix () "/E/code/feedback/mid_surround" 0
    (let* ((sound-list (reverse (ly::data *pure-atoms*))))
      (fplay (startn 3) startn4
	     (srt (srt-fun1 i))
	     (srt-env (srt-break2 (- 50 (* (expt line 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001))
		      (srt-break2 (- 50 (* (expt line2 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001)))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 (+ i 5)) 30) sound-list)
		    (nth (mod (sound-fun1 (+ i 12)) 31) sound-list))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (ly::duration sound) srt) 5 stop-in)
		       (min (/ (ly::duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	     (fader (fader *mid-f* line) (fader *mid-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 0 90))))
  (with-mix () "/E/code/feedback/mid_surround2" 0
    (let* ((sound-list (reverse (ly::data *pure-atoms*))))
      (fplay (startn 3) startn4
	     (srt (* (srt-fun1 i) .8))
	     (srt-env (srt-break2 (- 50 (* (expt line 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001))
		      (srt-break2 (- 50 (* (expt line2 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001)))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 (+ i 15)) 30) sound-list)
		    (nth (+ (mod (sound-fun1 (+ i 20)) 26) 13) sound-list))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (ly::duration sound) srt) 5 stop-in)
		       (min (/ (ly::duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	     (fader (fader *mid-f* line) (fader *mid-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 90 0))))
  (with-mix () "/E/code/feedback/mid_surround3" 0
    (let* ((sound-list (reverse (ly::data *pure-atoms*))))
      (fplay (startn 3) startn4
	     (srt (* (srt-fun1 i) 1.1))
	     (srt-env (srt-break2 (- 50 (* (expt line 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001))
		      (srt-break2 (- 50 (* (expt line2 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001)))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (+ (mod (sound-fun2 (+ i 5)) 30) 9) sound-list)
		    (nth (mod (sound-fun1 (- i 3)) 35) sound-list))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (ly::duration sound) srt) 5 stop-in)
		       (min (/ (ly::duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	     (fader (fader *mid-f* line) (fader *mid-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 4 - Remix
;;; Re-using Intro
  (with-mix () "/E/code/feedback/remix_surround" 0
    (let* ((sound-list (reverse (ly::data *pure-atoms*)))
	   (intro (format nil "~a~a" *src-dir* "intro.wav"))
	   (intro-noise (format nil "~a~a" *src-dir* "intro-noise.wav")))
      (fplay (startn 3) startn4
	     (srt (/ (srt-fun1 i) 2))
	     (file intro intro-noise)
	     (pure-sound (nth (mod (sound-fun2 (+ i 20)) 39) sound-list)
			 (nth (mod (sound-fun1 (+ i 21)) 39) sound-list))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (ly::duration pure-sound) srt) 5 stop-in)
		       (min (/ (ly::duration pure-sound2) srt) stop-in2))
	     (start "0 then (+ start rhythm)"
		    "0 then (+ start2 rhythm2)")
	     (reverse t)
	     (fader (fader *remix-f* line) (fader *remix-f* line2))
	     (amp (* (+ 3 line) fader) (* (+ 3 line) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2-noise i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2-noise i) mult) 5)))
	     (degree (nth (mod i 30) (procession 30 '(0 70 20 55 10 45 90)))
		     (- 90 (nth (mod i 32)
				(procession 32 '(0 70 20 55 10 45 90))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-mix () "/E/code/feedback/mid_dist_surround" 0
    (let* ((sound-list (loop for i in '(0 2 4 5 8 10) collect
			    (nth i (reverse (ly::data *distorted*))))))
      (fplay (startn 3) (startn 4)
	     (srt (srt-fun1 i))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 i) 6) sound-list)
		    (nth (mod (sound-fun1 i) 6) sound-list))
	     (stop-in (- (startn 4) time) (- (startn 4) time2))
	     (duration (min (/ (ly::duration sound) srt) 5 stop-in)
		       (min (/ (ly::duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (ly::peak sound)) (/ 1 (ly::peak sound2)))
	     (fader (fader *mid-dist-f* line) (fader *mid-dist-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.5)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.5)) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 90 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 5
;;; Intro again, but distorted ;)
;;; check if it is possible to get rid of the unedited samples
  (with-mix () "/E/code/feedback/last_surround" 0
    (sound-let
	((dist ()			;(:scaled-to 0.95)
	       (fplay (startn 4) (startn 5)
		      (sounds (reverse (ly::data *distorted*)))
		      (add (if (>= time (+ (startn 1) 15))
			       (* (- 1 (expt line 0.3)) 5) 0))
		      (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
			      (+ add (rthms1 i 2)))
		      (n (sound-fun2 (- i 1)))
		      (sound (nth n sounds)
			     (nth n sounds)
			     (nth n sounds))
		      (amp (+ .3 (if (= n 6) .2 0)))
		      (amp-env (env-fun1 (+ 80 (* line 20))))
		      (srt (srt-fun1 i) (srt-fun1 i) (srt-fun1 i))
		      (stop-in (- (startn 5) time 2)
			       (- (startn 5) time2 2)
			       (- (startn 5) time3 2))
		      (duration (min (/ (ly::duration sound) srt 0.95)
				     stop-in)
				(min (/ (ly::duration sound2) srt2 0.96)
				     stop-in2)
				(min (/ (ly::duration sound3) srt3 0.94)
				     stop-in3))
		      (degree 0 (+ 45 (* (- (mod i 2) .5) 90 (- 1 line2))) 90)
		      (printing nil nil))))
      (fplay (startn 4) (- (startn 5) 3)
	     (fader (fader *last-f* line))
	     (amp (* 8 fader))
	     (file dist)
	     (duration (- (startn 5) 3 (startn 4)))
	     (start (startn 4))
	     (channel 0 1)
	     (amp-env (env-expt .1 .1 nil t))
	     (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

(defun remix-rthm (i)
  (let* ((pt1 '(.4 .2 .1 .6 .2))
	 (pt2 '(.1 .5 .3 .2))
	 (morph (morph-patterns `(,pt1 ,pt2) (- (+ (startn 3) 20) startn2)
				nil t nil nil ))
	 (len (length morph)))
  (nth (mod i len) morph)))

(defun remix-srt (i)
  (nth (mod i 6) '(.8 .81 .8 .78 .82 .79)))

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/noise_rhythm_surround.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (fplay startn2 (+ (startn 3) 10)
	 (time .05)
	 (file '"/E/feed_it/Samples/noise_06_continuous_x2.wav")
	 (amp-env '(0 0 1 1 100 0))
	 (amp (dry-wet (- 1 (remix-rthm i)) 0 line))
	 (duration (* (remix-rthm i) .4))
	 (rhythm duration)
	 (reverse (= 0 (mod i 2)))
	 (start line)
	 (end (+ line duration))
	 (channel 0 1)
	 (degree 0 90)))

#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/break_remix.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (fplay startn2 (+ (startn 3) 20)
	 (file '"/E/code/feedback/break.wav")
	 (amp-env '(0 0 99 1 100 0))
	 (amp (dry-wet 1 (- 1 (remix-rthm i)) .5))
	 (duration (* (remix-rthm i) .4))
	 (rhythm duration)
	 (reverse (= 1 (mod i 2)))
	 (start time)
	 (end (+ time duration))
	 (channel 0 1)
	 (degree 0 90)))

#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/break02_remix.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (fplay startn2 (+ (startn 3) 20)
	 (file '"/E/code/feedback/break02.wav")
	 (amp-env '(0 0 99 1 100 0))
	 (amp (dry-wet 1 (- 1 (remix-rthm i)) .5))
	 (duration (* (remix-rthm i) .4))
	 (rhythm duration)
	 (reverse (= 1 (mod i 2)))
	 (start time)
	 (end (+ time duration))
	 (channel 0 1)
	 (degree 0 90)))

;; REMIX
;;; links und rechts unterschiedlich reversen
#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/just_feed_it-remix_surround.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (with-mix () "/E/code/feedback/break-remix" 0
    (fplay (startn 0) (- (startn 6) 7)
	   (file '"/E/code/feedback/break.wav")
	   (srt (remix-srt i))
	   (duration (min (- (- (startn 6) 6) time)
			       (remix-rthm i)))
	   (rhythm duration)
	   (reverse (= 1 (mod i 2)))
	   (start time)
	   (end (+ time duration))
	   (channel 0 1)
	   (degree 0 90)
	   ))
  (with-mix () "/E/code/feedback/intro-remix" 0
    (fplay (startn 0) (- (startn 6) 7)
	   (file '"/E/code/feedback/intro.wav")
	   (duration (min (- (- (startn 6) 6) time)
			       (nth (mod i 8) '(89 10.9 72.1 7.3 13 4 23 10000))))
	   (rhythm duration)
	   (amp-env '(0 0  .5 1  99.5 1  100 0))
	   (reverse (= 1 (mod i 2)))
	   (start time)
	   (end (+ time duration))
	   (channel 0 1)
	   (degree 0 90)
	   ))
  (with-mix () "/E/code/feedback/continuo-remix" 0
    (fplay (startn 0) (- (startn 6) 7)
	   (file '"/E/code/feedback/continuo.wav")
	   (duration (min (- (- (startn 6) 6) time)
			  (nth (mod i 10) '(89 10.9 39.1 6 28 7.3 20 4 26 1000)))
		     (min (- (- (startn 6) 6) time)
			  (nth (mod i 10) '(89 10.9 40.1 4.6 28.4 7.3 20 5 25 1000))))
	   (rhythm duration duration2)
	   (amp-env '(0 0  .5 1  99.5 1  100 0))
	   (reverse (= 1 (mod i 2)))
	   (start time time2)
	   (end (+ time duration) (+ time2 duration2))
	   (channel 0 1)
	   (degree 0 90)
	   )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF score.lsp
