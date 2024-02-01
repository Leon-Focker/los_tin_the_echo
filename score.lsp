;; * SCORE
;;; Generation of the piece happens here, check out the documentation for each
;;; function to find out how things work :]

(in-package :layers)

(format t "~&processing SCORE")

;; TODO: *distorted* contains raw samples (bc folder inside folder) :/

(defparameter *bass-file-1* "/E/Keks_Feedback/samples/distorted/cookies_distorted_03.wav")
(defparameter *bass-file-2* "/E/Keks_Feedback/samples/distorted/cookies_distorted_01a.wav")
(defparameter *bass-file-3* "/E/Keks_Feedback/samples/distorted/cookies_distorted_05a.wav")

;; ** form
;; 1 intro (pure)
;; 2 rhythmen (pure + percussive)
;; 3 short noise block (noise)
;; 4 rhythmic noise blocks (dist + noise)
;; 5 distortion (dist)
;; 6 short outro (quiet dist)
(defparameter *form* (let* ((st (make-fractal-structure '(1 2 3 4 5 6)
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
			    (form (cdr (reverse (data st)))))
		       ;;(visualize-structure st 1 "/E/code/feedback/structure_comp1.png")
		       form))

(defparameter *sections* (first *form*))

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

;; ** material

;; *** abstractions

;; abstraction for window-transitions
(defun wt (length levels &optional (e 0.8))
  (window-transitions length levels e 1))

(defparameter *amp-env01*
  (append 
   (loop for i from 0 to 80 collect i collect (expt (/ i 80) 0.3))
   (loop for i from 81 to 100 collect i collect (expt (/ (- 100 i) 20) 0.3))))

(defun env-fun1 (breakpoint &optional (exponent 0.3))
  (let ((bp (max 0 (min 100 (round breakpoint)))))
    (append
     (if (= bp 0) '(0 1)
	 (loop for i from 0 to bp
	    collect i collect (expt (/ i bp) exponent)))
     (loop for i from (1+ bp) to 100
	collect i collect (expt (/ (- 100 i) (- 100 bp)) exponent)))))

;; base should be between 0 and 1
(defun env-expt (pow &optional (base 0) reverse? flip?)
  (unless (<= 0 base 1)
    (warn "base should be between or equal to 0 and 1 but is: ~a" base))
  (loop for i from 0 to 100
     for val = (expt (/ (if reverse? (- 100 i) i) 100) pow)
     collect i collect
       (+ (* (- 1 base) (if flip? (- 1 val) val))
	  base)))

;; srt-env function
(defun srt-break (breakpoint new-val &optional (br-len .001))
  (unless (and (numberp breakpoint) (numberp new-val))
    (error "all arguments in srt-break should be numbers"))
  (cond ((<= breakpoint 0) `(0 ,new-val 100 ,new-val))
	((>= breakpoint (- 100 br-len)) `(0 0 100 0))
	(t `(0 0 ,breakpoint 0 ,(+ breakpoint br-len) ,new-val 100 ,new-val))))

(defun srt-break2 (breakpoint old-val &optional (br-len .001))
  (unless (and (numberp breakpoint) (numberp old-val))
    (error "all arguments in srt-break should be numbers"))
  (cond ((<= breakpoint 0) `(0 0 100 0))
	((>= breakpoint (- 100 br-len)) `(0 ,old-val 100 ,old-val))
	(t `(0 ,old-val ,breakpoint ,old-val ,(+ breakpoint br-len) 0 100 0))))

;; *** patterns

(defparameter *pattern1* '(0.5 0.75 0.5 0.75 0.5 0.75 0.75))
(defparameter *pattern2* '(0.5 0.75 0.75 0.5 0.75 0.5 0.75))
(defparameter *pattern3* '(0.75 0.5 0.75 0.5 0.75 0.5 0.5 0.75))
(defparameter *pattern4* '(1/2 2/3 1/2 1/3 1/3 1/3 3/4))
(defparameter *pattern5* '(0.2 0.45 0.2 0.5 0.45))
(defparameter *pattern6* '(1 1/6 1/6 0.2 1/3))
(defparameter *pattern7* '(0.2 0.2 0.4))

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
       (quiet-len (length (data *quiet-atoms*)))
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
       (ls7 (wt (* len 2) (length (data *pure-atoms*)))))
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
    (nth (mod i len7) ls7))
  (defun br-rthms8 (i)
    (nth (mod i len8) ls8))
  (defun br-rthms9 (i)
    (nth (mod i len9) ls9))
  (defun br-rthms10 (i)
    (nth (mod i len10) ls10))
  (defun br-rthms11 (i)
    (nth (mod i len10) (reverse ls10)))
  (defun br-rthms12 (i)
    (nth (mod i len9) (reverse ls9))))

;; ** Generation

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
	     :output (format nil "~a~a" *fb-src-dir* "just_feed_it.wav")
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 1, Intro
  (with-mix () (format nil "~a~a" *fb-src-dir* "intro") 0
    (let* ((sound-list (reverse (data *quiet-atoms*))))
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
			 (sound (nth (mod (sound-fun2 i) 18) sound-list)
				(nth (mod (sound-fun1 i) 18) sound-list))
			 (stop-in (- startn2 time) (- startn2 time2))
			 (duration (min (/ (duration sound) srt) 5 stop-in)
				   (min (/ (duration sound2) srt) stop-in2))
			 (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2)))
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
			 (amp-env
			  (cond ((>= srt 1) (env-fun1 (+ 10 rhythm) .5))
				((>= srt .9)  (env-fun1 (* line 90) 2))
				((< .68 srt .72)  (env-fun1 (+ 10 (* line 30))
							    .2))
				(t (env-fun1 (- 80 (* 70 (expt line .5)))))))
			 (degree 0 90)
			 (printing t))))
  (with-mix () (format nil "~a~a" *fb-src-dir* "intro-noise") 0
    (let* ((sound-list (reverse (data *quiet-atoms*))))
      (fplay (startn 0) startn2 ;;(+ (startn 1) 20)
	     (srt (srt-fun1 i))
	     (amp-env (if (<= 95 time) '(0 0 1 1  99 1 100 0)
			  (env-fun1 (- 90 (* 70 (expt line .5))) 1.2))
		      (if (<= 95 time) '(0 0 1 1  99 1 100 0)
			  (env-fun1 (- 90 (* 70 (expt line .5))) 1.2))
		      (env-fun1 75 1.2)
		      (env-fun1 75 1.2))
	     (sound *noise-floor*)
	     (pure-sound (nth (mod (sound-fun2 i) 18) sound-list)
			 (nth (mod (sound-fun1 i) 18) sound-list))
	     (start (+ (* (/ (sound-fun2 i) 18) 80) 1)
		    (+ (* (/ (sound-fun1 i) 18) 80) 1))
	     (dur (min (/ (duration pure-sound) srt) 5)
		  (/ (duration pure-sound2) srt))
	     (duration (* dur 1.2)
		       (* dur2 1.2)
		       (dry-wet dur 1 (expt line3 5))
		       (dry-wet dur2 1 (expt line4 5)))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ dur
			(min (* (rest-fun2-noise i) mult) 5))
		     (+ dur2
			(min (* (rest-fun2-noise i) mult) 5))
		     (+ (dry-wet dur 1 (expt line3 5)) (if (> time3 170) .5 i))
		     (+ (dry-wet dur2 1 (expt line4 5)) (if (> time4 170) .5 i)))
	     (amp-mult (/ 1 (peak pure-sound)) (/ 1 (peak pure-sound2))
		       (/ 1 (peak pure-sound)) (/ 1 (peak pure-sound2)))
	     (amp-fade (if (<= time (startn 1)) 1
			   (expt (/ (- time (startn 1))
				    (- startn2 (startn 1)))
				 2))
		       (if (<= time2 (startn 1)) 1
			   (expt (/ (- time2 (startn 1))
				    (- startn2 (startn 1)))
				 2)))
	     (fader (fader *intro-n-f* line) (fader *intro-n-f* line2)
		    (fader *intro-n-f* line3) (fader *intro-n-f* line4))
	     (amp (* (dry-wet 0.05 .3 line) amp-mult amp-fade .02 fader)
		  (* (dry-wet 0.05 .3 line2) amp-mult amp-fade .015 fader2)
		  (* (dry-wet 0.02 .3 line3) fader3 );amp-fade2)
		  (* (dry-wet 0.02 .3 line4) fader4 ));amp-fade2))
	     (degree 0 90 0 90)
	     (printing nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2
;;; Building on Intro
;;; new srt-funs
  (with-mix () (format nil "~a~a" *fb-src-dir* "continuo") 0
    (fplay (+ (startn 1) 5) startn2
	   (sounds (reverse (data *quiet-atoms*)))
	   (add (if (>= time (+ (startn 1) 15))
		    (* (- 1 (expt line 0.3)) 5) 0))
	   (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
		   (+ add (rthms1 i 2)))
	   (sound (nth (sound-fun2 i) sounds)
		  (nth (sound-fun2 i) sounds)
		  (nth (sound-fun2 i) sounds))
	   (fader (fader *continuo-f* line) (fader *continuo-f* line2)
		  (fader *continuo-f* line3))
	   (amp (* .9 fader) (* .91 fader2) (* .91 fader3))
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
	   (duration (min (/ (duration sound) srt)
			  stop-in)
		     (min (/ (duration sound2) srt2)
			  stop-in2)
		     (min (/ (duration sound3) srt3)
			  stop-in3))
	   (amp-env
	    (cond ((>= srt 1) (env-fun1 (+ 10 rhythm) .5))
		  ((>= srt .9)  (env-fun1 (* line 90) 2))
		  ((< .68 srt .72)  (env-fun1 (+ 10 (* line 30))
					      .2))
		  (t *amp-env01*)))
	   (degree 0 (+ 45 (* (- (mod i 2) .5) 90 (- 1 line2))) 90)
	   (printing t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-mix () (format nil "~a~a" *fb-src-dir* "continuo2") 0
    (fplay (+ (startn 1) 5) (startn 2)
	   (sounds (reverse (data *quiet-atoms*)))
	   (add (if (>= time (+ (startn 1) 15))
		    (* (- 1 (expt line 0.3)) 5) 0))
	   (srt (srt-fun2 i))
	   (rhythm (+ add (rthms1 i 0) (* (- srt 0.75) 1/3)))
	   (sound (nth (if (>= time 173) (sound-fun2 i) (sound-fun1 i))
		       sounds))
	   (duration (min (/ (duration sound) srt)
			  (- (startn 2) time)))
	   (fader (fader *continuo2-f* line))
	   (amp (* (if (> line .42) 0.9 0) fader))
	   (amp-env *amp-env01*)
	   (degree 45)
	   (printing t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-mix () (format nil "~a~a" *fb-src-dir* "bass") 0
    (let ((file *bass-file-1*)
	  (file2 *bass-file-2*)
	  (time (* 0.792 (startn 2)))
	  (time2 (* 0.8059 (startn 2)))
	  (srt 0.023181288)
	  (srt2 0.0487395))
      (samp0 file time :amp (* 1.3 (fader *bass-f*)) :amp-env *amp-env01*
	     :degree 45 :srt srt2
	     :duration (min (/ (soundfile-duration file) srt)
			    (- startn2 time)))
      (samp0 file2 time2 :amp (* 1 (fader *bass-f*)) :amp-env *amp-env01* 
	     :degree 45 :srt srt2
	     :duration (min (/ (soundfile-duration file2) srt2)
			    (- startn2 time2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 3
  (with-mix () (format nil "~a~a" *fb-src-dir* "break") 0
    (let* ((sound-list (reverse (data *noise*))))
      (fplay startn2 (startn 3)
	     (sound (nth (mod i 10) sound-list)
		    (nth (mod (+ i 1) 10) sound-list)
		    (nth (mod (+ i 2) 10) sound-list)
		    (nth (mod (+ i 3) 10) sound-list))
	     (rhythm (br-rthms1 i)
		     (br-rthms2 i)
		     (br-rthms3 i)
		     (br-rthms4 i))
	     (duration (rationalize (* rhythm (- 1 (expt line 2))))
		       (rationalize (* rhythm2 (- 1 (expt line2 2))))
		       (rationalize (* rhythm3 (- 1 (expt line3 2))))
		       (rationalize (* rhythm4 (- 1 (expt line4 2)))))
	     (start "0 then (+ start duration)"
		    "0 then (mirrors (+ start duration2) 0 4)"
		    "0 then (mirrors (+ start duration3) 0 2.5)"
		    "0 then (mirrors (+ start duration4) 0 4.5)")
	     (fader (fader *break-f* line) (fader *break-f* line2)
		    (fader *break-f* line3) (fader *break-f* line4))
	     (amp (* 0.7 fader) (* 0.7 fader2) (* 0.7 fader3) (* 0.7 fader4))
	     (amp-env *amp-env01*)
	     (degl (* 90 line 2) (* 90 line2 2) (* 90 line3 2) (* 90 line4 2))
	     (degree (mirrors (+ 0 degl) 0 90)
		     (mirrors (- 30 degl2) 0 90)
		     (mirrors (- 60 degl3) 0 90)
		     (mirrors (+ 90 degl4) 0 90))
	     (printing nil nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rhythms, leaking into next part.
  (with-mix () (format nil "~a~a" *fb-src-dir* "break02") 0
    (let* ((sound-list (reverse (data *noise*))))
      (fplay startn2 startn3 
	     (sound (nth (+ 4 (* (round (funcall
					 (make-list-into-function
					  (fibonacci-transition 10) 1)
					 (- 1 line)))
				 2))
			 sound-list))
	     (rhythm (br-rthms7 i)
		     (br-rthms8 i)
		     (br-rthms9 i)
		     (br-rthms10 i))
	     (duration (rationalize
			(* rhythm (+ .001 (* 0.999 (- 1 (expt line .6)))))))
	     (fader (fader *break02-f* line) (fader *break02-f* line2)
		    (fader *break02-f* line3) (fader *break02-f* line4))
	     (amp-mult (+ 0 (* .71 (- 1 (expt line 1.2)))))
	     (amp (* amp-mult fader) (* amp-mult fader2)
		  (* amp-mult fader3) (* amp-mult fader4))
	     (start "1 then (mirrors (+ start duration) 1 4)"
		    "2 then (mirrors (+ start duration) 1 4)"
		    "3 then (mirrors (+ start duration) 1 4)"
		    "4 then (mirrors (+ start duration) 1 4)")
	     (amp-env *amp-env01*)
	     (degl (* 90 line 2) (* 90 line2 2) (* 90 line3 2) (* 90 line4 2))
	     (degree (mirrors degl 0 90) (mirrors (+ 30 degl2) 0 90)
		     (mirrors (+ 60 degl3) 0 90) (mirrors (- 90 degl4) 0 90))
	     (printing nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rhythms during 4th part.
  (with-mix () (format nil "~a~a" *fb-src-dir* "rhythms") 0
    (let* ((sound-list (reverse (data *noise*))))
      (fplay startn3 (+ startn4 52.5)
	     (sound (nth 6 sound-list))
	     (rthm (* (br-rthms11 i) (- 1 (* (expt line 2) .4)))
		   (* (br-rthms12 i) (- 1 (* (expt line 2) .4))))
	     (rhythm rthm rthm rthm rthm rthm2 rthm2 rthm2 rthm2)
	     (srt (dry-wet .2 8 (expt line 2)))
	     (duration (rationalize (if (> line .3) (* rhythm .01)
					(* rhythm (dry-wet 0.01 .2 (- line .6))))))
	     (fader (fader *rhythms-f* line))
	     (amp (* (dry-wet .03 .15 (expt line 1.2)) fader))
	     (start "1 then (mirrors (+ start duration) 1 4)"
		    "2 then (mirrors (+ start duration) 1 4)"
		    "3 then (mirrors (+ start duration) 1 4)"
		    "4 then (mirrors (+ start duration) 1 4)"
		    "1 then (mirrors (+ start duration) 1 4)"
		    "2 then (mirrors (+ start duration) 1 4)"
		    "3 then (mirrors (+ start duration) 1 4)"
		    "4 then (mirrors (+ start duration) 1 4)")
	     (amp-env (env-fun1 10 2))
	     (degl (* 90 line 5) (* 90 line 5) (* 90 line 5) (* 90 line 5)
		   (* 90 line 8) (* 90 line 8) (* 90 line 8) (* 90 line 8))
	     (degree (mirrors degl 0 90) (mirrors (+ 30 degl2) 0 90)
		     (mirrors (+ 60 degl3) 0 90)
		     (mirrors (- 90 degl4) 0 90)
		     (mirrors degl 0 90) (mirrors (+ 30 degl2) 0 90)
		     (mirrors (+ 60 degl3) 0 90)
		     (mirrors (- 90 degl4) 0 90))
	     (printing nil nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (with-mix () (format nil "~a~a" *fb-src-dir* "bridge") 0
    (let* ((sound-list (reverse (data *pure-atoms*))))
      (fplay startn2 (startn 3)
	     (srt (srt-fun1 i))
	     (amp-env (env-fun1 80))
	     (sound (nth (mod (sound-fun2 i) 39) sound-list)
		    (nth (mod (sound-fun1 i) 39) sound-list))
	     (duration (min (/ (duration sound) srt) 5)
		       (/ (duration sound2) srt))
	     (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2)))
	     (fader (fader *bridge-f* line) (fader *bridge-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) fader2))
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 4
;;; Condinuo again, but not quiet
  (with-mix () (format nil "~a~a" *fb-src-dir* "mid") 0
    (let* ((sound-list (reverse (data *pure-atoms*))))
      (fplay (startn 3) startn4
	     (srt (srt-fun1 i))
	     (srt-env (srt-break2 (- 50 (* (expt line 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001))
		      (srt-break2 (- 50 (* (expt line2 2) 50))
				  (srt-fun1 (1- i))
				  (+ (* (- 1 (expt line 2)) 1.5) .001)))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 i) 39) sound-list)
		    (nth (mod (sound-fun1 i) 39) sound-list))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (duration sound) srt) 5 stop-in)
		       (min (/ (duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2)))
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
  (with-mix () (format nil "~a~a" *fb-src-dir* "remix") 0
    (let* ((sound-list (reverse (data *pure-atoms*)))
	   (intro (format nil "~a~a" *src-dir* "intro.wav"))
	   (intro-noise (format nil "~a~a" *src-dir* "intro-noise.wav")))
      (fplay (startn 3) startn4
	     (srt (/ (srt-fun1 i) 2))
	     (file intro intro-noise)
	     (pure-sound (nth (mod (sound-fun2 i) 39) sound-list)
			 (nth (mod (sound-fun1 i) 39) sound-list))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (duration pure-sound) srt) 5 stop-in)
		       (min (/ (duration pure-sound2) srt) stop-in2))
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
  (with-mix () (format nil "~a~a" *fb-src-dir* "remixed-noise") 0
    (let* ((sound-list (reverse (data *pure-atoms*)))
	   (intro (format nil "~a~a" *src-dir* "intro.wav"))
	   (intro-noise (format nil "~a~a" *src-dir* "intro-noise.wav")))
      (fplay (startn 3) startn4
	     (srt (/ (srt-fun1 i) 2))
	     (file intro-noise intro-noise)
	     (pure-sound (nth (mod (sound-fun2 (+ i 13)) 39) sound-list)
			 (nth (mod (sound-fun1 (+ i 25)) 39) sound-list))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (stop-in (- startn4 time) (- startn4 time2))
	     (duration (min (/ (duration pure-sound) srt) 5 stop-in)
		       (min (/ (duration pure-sound2) srt) stop-in2))
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
  (with-mix () (format nil "~a~a" *fb-src-dir* "mid_dist") 0
    (let* ((sound-list (loop for i in '(0 2 4 5 8 10) collect
			    (nth i (reverse (data *distorted*))))))
      (fplay (startn 3) (startn 4)
	     (srt (srt-fun1 i))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 i) 6) sound-list)
		    (nth (mod (sound-fun1 i) 6) sound-list))
	     (stop-in (- (startn 4) time) (- (startn 4) time2))
	     (duration (min (/ (duration sound) srt) 5 stop-in)
		       (min (/ (duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2)))
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
  (with-mix () (format nil "~a~a" *fb-src-dir* "last") 0
    (sound-let
	((dist ()			;(:scaled-to 0.95)
	       (fplay (startn 4) (startn 5)
		      (sounds (reverse (data *distorted*)))
		      (add (if (>= time (+ (startn 1) 15))
			       (* (- 1 (expt line 0.3)) 5) 0))
		      (rhythm (+ add (rthms1 i 0)) (+ add (rthms1 i 1))
			      (+ add (rthms1 i 2)))
		      (n (sound-fun2 i))
		      (sound (nth n sounds)
			     (nth n sounds)
			     (nth n sounds))
		      (amp (+ .3 (if (= n 6) .2 0)))
		      (amp-env (env-fun1 (+ 80 (* line 20))))
		      (srt (srt-fun1 i) (srt-fun1 i) (srt-fun1 i))
		      (stop-in (- (startn 5) time 2)
			       (- (startn 5) time2 2)
			       (- (startn 5) time3 2))
		      (duration (min (/ (duration sound) srt)
				     stop-in)
				(min (/ (duration sound2) srt2)
				     stop-in2)
				(min (/ (duration sound3) srt3)
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
  (with-mix () (format nil "~a~a" *fb-src-dir* "bass-last") 0
    (sound-let
	((bass ()
	       (let ((file *bass-file-3*)
		     (file2 *bass-file-1*)
		     (time (+ (* 0.5 (- (startn 4) (startn 3))) (startn 3) 3.5))
		     (time2 (+ (* 0.5 (- (startn 4) (startn 3))) (startn 3) 3))
		     (srt 0.04312288)
		     (srt2 0.036894996))
		 (samp0 file time :amp 1 :amp-env *amp-env01* :srt srt :degree 45
			:start 2.12
			:duration (min (/ (soundfile-duration file) srt)
				       (- (startn 5) time)))
		 (samp0 file2 time2 :amp 1.3 :amp-env *amp-env01* :srt srt2 :degree 45
			:start .25
			:duration (min (/ (soundfile-duration file2) srt2)
				       (- (startn 5) time2))))))
      (fplay (startn 3) (- (startn 5) 3)
	     (fader (fader *bass-last-f* line))
	     (amp fader)
	     (file bass)
	     (duration (- (startn 5) 3 (startn 3)))
	     (start (startn 3))
	     (channel 0 1)
	     (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Teil 6
;;; Intro is Outro :P
  (with-mix () (format nil "~a~a" *fb-src-dir* "outro") 0
    (let* ((sound-list (reverse (data *quiet-atoms*))))
      (fplay (- (startn 5) 3) (- (startn 6) 7)
	     (srt (srt-fun1 i))
	     (amp-env (env-fun1 (- 80 (* 70 (expt line .5)))))
	     (sound (nth (mod (sound-fun2 i) 18) sound-list)
		    (nth (mod (sound-fun1 i) 18) sound-list))
	     (stop-in (- (startn 6) time) (- (startn 6) time2))
	     (duration (min (/ (duration sound) srt) 5 stop-in)
		       (min (/ (duration sound2) srt) stop-in2))
	     (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2)))
	     (fader (fader *outro-f* line) (fader *outro-f* line2))
	     (amp (* (dry-wet 0.9 amp-mult (* line 0.3)) fader)
		  (* (dry-wet (* line 0.7) amp-mult2 (* line2 0.3)) fader2))
	     (reverse t)
	     (mult (+ 1 (* (expt line 0.3) 2)))
	     (rhythm (+ duration
			(min (* (rest-fun2 i) mult) 5))
		     (+ duration2
			(min (* (rest-fun2 i) mult) 5)))
	     (degree 0 90))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

;; ** JUNK

;;; all this was used to generate soundfiles that could then be used in the main
;;; score, as "filler material" (doubling sounds) or just to try things out.
;;; Tiding this does not make much sense, which is why it is commented out.

#|

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
			  :output "/E/feed_it/Samples/noise_rhythm.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (fplay startn2 (+ (startn 3) 10)
	 (file '"/E/feed_it/Samples/noise_06_continuous_x2.wav")
	 (amp-env '(0 0 1 1 100 0))
	 (amp (dry-wet (- 1 (remix-rthm i)) 0 line))
	 (duration (* (remix-rthm i) .4))
	 (rhythm duration)
	 (reverse (= 1 (mod i 2)))
	 (start line)
	 (end (+ line duration))
	 (channel 0 1)
	 (degree 0 90)))

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/Samples/break_remix.wav"
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

(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/Samples/break02_remix.wav"
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
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/Samples/just_feed_it-remix.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (with-mix () "/E/feed_it/Samples/break-remix" 0
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
  (with-mix () "/E/feed_it/Samples/intro-remix" 0
    (fplay (startn 0) (- (startn 6) 7)
	   (file '"/E/code/feedback/intro.wav")
	   (duration (min (- (- (startn 6) 6) time)
			       (nth (mod i 8) '(89 10.9 72.1 7.3 13 4 23 10000))))
	   (rhythm duration)
	   (amp-env '(0 0  .5 1  99.5 1  100 0))
	   (reverse (= 1 (mod i 2)))
	   (start time)
	   (amp .99)
	   (end (+ time duration))
	   (channel 0 1)
	   (degree 0 90)
	   ))
  (with-mix () "/E/feed_it/Samples/continuo-remix" 0
    (fplay (startn 0) (- (startn 6) 7)
	   (file '"/E/feed_it/Samples/continuo.wav")
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


(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/Samples/remix-noise.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (fplay (startn 0) (- (startn 6) 7)
	 (file '"/E/code/feedback/intro-noise.wav")
	 (duration (min (- (- (startn 6) 6) time)
			(nth (mod i 8) '(89 10.9 72.1 7.3 13 4 23 10000))))
	 (rhythm duration)
	 (amp-env '(0 0  .5 1  99.5 1  100 0))
	 (reverse (= 1 (mod i 2)))
	 (start time)
	 (amp .99)
	 (end (+ time duration))
	 (channel 0 1)
	 (degree 0 90)
	 ))

|#

#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/distorted_05a_x0.06486647.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (clm::simple-src
   "/E/Keks_Feedback/samples/distorted/cookies_distorted_05a.wav"
   0 0.06486647 :width 100))

#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/feed_it/Samples/noise_06_continuous_x2.wav"
			  :channels 2 :play nil :scaled-to 0.98
			  :force-recomputation nil)
  (clm::simple-src
   "/E/feed_it/Samples/noise_06_continuous.wav"
   0 2 :width 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+nil(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/disruptor.wav"
			  :channels 2 :play nil :scaled-to 0.98)
  ;;(with-mix () "/E/code/feedback/disruptor" 0
  (let* ((sound-list (reverse (data *noise*))))
    (fplay (startn 3) (startn 4)
	   (time-mult 5)
	   (time (+ 5 (startn 3)))
	   (sound (nth (mod (sound-fun2 i) 10) sound-list)
		  (nth (mod (sound-fun1 i) 10) sound-list)
		  (nth (mod (sound-fun2 (+ i 3)) 10) sound-list)
		  (nth (mod (sound-fun1 (+ i 4)) 10) sound-list)
		  (nth (mod (sound-fun2 (+ i 7)) 10) sound-list)
		  (nth (mod (sound-fun1 (+ i 8)) 10) sound-list))
	   (duration (dry-wet .8 2 line))
	   (amp-mult (/ 1 (peak sound)) (/ 1 (peak sound2))
		     (/ 1 (peak sound3)) (/ 1 (peak sound4))
		     (/ 1 (peak sound5)) (/ 1 (peak sound6)))
	   (fader (fader *mid-dist-f* line) (fader *mid-dist-f* line2))
	   (amp (* .3 amp-mult fader)
		(* .3 amp-mult2 fader2)
		(* .3 amp-mult3 fader2)
		(* .3 amp-mult4 fader2)
		(* .3 amp-mult5 fader2)
		(* .3 amp-mult6 fader2))
	   (amp-env '(0 1  100 1))
	   (start (+ 1 line))
	   (rthm (* (rthms1 (1+ i) 2) time-mult)
		 (* (rthms1 (1+ i) 1) time-mult))
	   (rhythm rthm rthm2 rthm rthm2 rthm rthm2)
	   (degree 0 90 0 90 0 90))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF score.lsp
