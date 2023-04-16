;; * SPATIAL
;;; the purpose of this file is to automatically generate reaper files that
;;; spatialize soundfiles using the IEM ambisonics (or other spatial audio)
;;; plugins.
;;; check out #'write-spatial-reaper-file below, if you want to use it.

;;; TODO
;;; further testing required, wheter this works for other people on other
;;; systems
;;; multichannel implementation? or stereo to mono (with auto balance)
;;; some edge cases of env-mod might still be buggy (?)
;;; envelopes for angle and elevation are not 100% intuitive. change?

;; ** general

(in-package :sc)

;; to load from same dir:
(defparameter *src-dir*
  (directory-namestring (truename *load-pathname*)))

;; load a regex library
(ql:quickload "cl-ppcre")

;; *** read-file
;;; read an entire file (not just an s-expression) a into string and return it
(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;; this is the (partially binary) data for the respective plugin, as it appears
;; in a reaper project file:
(defparameter *iem-stereo-encoder*
  (read-file (format nil "~a~a" *src-dir* "iem-stereo-encoder.txt")))
(defparameter *blue-ripple-decoder*
  (read-file (format nil "~a~a" *src-dir* "blue-ripple.txt")))
;; these IDs should be unique but it's probably not neccessary to change them:
(defparameter *encoder-fxid* "{21BA0349-8E33-48D7-94BA-D648B035A6FF}")
(defparameter *decoder-fxid* "{8898DF47-1C03-44CF-878F-50DE85568C55}")

;; only thouch these if you know what you're doing:
(defparameter *spatial-reaper-tempo* 60)
;; if nil - use individual-duration
;; if > 0 - set every env to this duration :duration t
;; if < 0 - end at (abs) of this time :end-times nil
(defparameter *spatial-reaper-duration*  nil)
(defparameter *spatial-reaper-use-starts* nil)

;; ** Project
;;; first create a reaper project that contains all samples:

;; *** make-reaper-items4
;; every soundfile gets its own track, start-times are given individually
(defun make-reaper-items4 (sndfiles &optional (start-times '(0)))
  (let ((items (apply #'make-reaper-items-aux
			      (list sndfiles nil 0.0
				    :num-tracks (length sndfiles)
				    :fade-in 0.0
				    :fade-out 0.0))))
    ;; must set start-times by hand now, as we had no rhythms
    (loop for item in items and i from 0 do
      (setf (start-time item) (nth (mod i (length start-times)) start-times)))
    items))

;; *** make-spatial-reaper-file
;;; make reaper-project holding the files to be spatialized
(defun make-spatial-reaper-file (soundfiles &key (start-times '(0))
					      (samplerate 48000))
  (let* ((items (make-reaper-items4 soundfiles start-times)))
    (make-reaper-file 'spatial items
		      :tempo *spatial-reaper-tempo*
		      :samplerate samplerate)))

;; ** Insert changes

;; *** small adjustments

(defun set-master-channels (string &optional (number-of-master-channels 16))
  (let* ((scan (ppcre:create-scanner "MASTER_NCH [0-9]+")))
    (ppcre:regex-replace-all scan string
			     (format nil "MASTER_NCH ~a"
				     number-of-master-channels))))

(defun set-track-channels (string &optional (number-of-track-channels 16))
  (let* ((scan (ppcre:create-scanner "NCHAN [0-9]+")))
    (ppcre:regex-replace-all scan string
			     (format nil "NCHAN ~a"
				     number-of-track-channels))))

;; *** insert plugins

;; **** insert-plugins-all-tracks
;;; insert plugin on every track except master
(defun insert-plugin-all-tracks
    (string &optional (plugin-data *iem-stereo-encoder*))
  (let* ((scan (ppcre:create-scanner "MAINSEND.{6,10}?<ITEM"
				     :single-line-mode t)))
    (ppcre:regex-replace-all
     scan
     string
     (format nil "MAINSEND 1 0 ~&~a~&<ITEM"
	     (format nil
		     (read-file (format nil "~a~a" *src-dir* "track-fx.txt"))
		     plugin-data
		     *encoder-fxid*)))))

;; **** insert-plugin-master
(defun insert-plugin-master
    (string &optional (plugin-data *blue-ripple-decoder*))
  (let* ((scan (ppcre:create-scanner
		"MASTER_SEL [0-9]+.{2,6}?<MASTERPLAYSPEEDENV"
		:single-line-mode t)))
    (ppcre:regex-replace-all
     scan
     string
     (format nil "MASTER_SEL 0~&~a~&<MASTERPLAYSPEEDENV"
	     (format nil (read-file (format nil "~a~a" *src-dir* "master-fx.txt"))
		     plugin-data
		     *decoder-fxid*)))))

;; *** faders
(defun set-all-faders (string &optional (set-to .5))
  (let* ((scan (ppcre:create-scanner "AUTOMODE ..{2,6}?VOLPAN [0-9]+?\[.]?[0-9]*"
				     :single-line-mode t)))
    (ppcre:regex-replace-all scan string
			     (format nil "AUTOMODE 0~&    VOLPAN ~a"
				     set-to))))

;; ** spatial-snfile class
;;; like a sc::sndfile but with envelopes for elevation and angle

(defclass spatial-sndfile (sndfile)
  ((angle-env :accessor angle-env :type list :initarg :angle-env
	      :initform '(0 .5  100 .5))
   (elevation-env :accessor elevation-env :type list :initarg :elevation-env
		  :initform '(0 .5  100 .5))
   ;; the string that will be printed in the reaper file
   (astring :accessor astring :type string
            ;; read in the text for an item
            :initform (read-file (format nil "~a~a" *src-dir* "astring.txt")))
   (start-time :accessor start-time :type number :initarg :start-time
	       :initform 0)))

;; *** make-spatial-sndfile
;;; not meant to be called from within a sndfile-palette object, so simple init
(defun make-spatial-sndfile (path &key (angle-env '(0 .5  100 .5))
				    (elevation-env '(0 .5  100 .5))
				    id data duration end (start 0.0)
				    (frequency nil)
				    (amplitude 1.0)
				    (start-time 0))
  (make-instance 'spatial-sndfile
		 :id id :data data :path path :duration duration
		 :angle-env angle-env :elevation-env elevation-env
		 :frequency frequency :end end :start start
		 :amplitude amplitude :start-time start-time))

;; *** env-mod
;;; modulo for envelope curves, using linear interpolation to create
;;; intermedeate points
(defun env-mod-aux (last-x last-y x y)
  (let* ((yfloordiff (- (floor y) (floor last-y)))
	 (pos? (> yfloordiff 0))
	 (ydiff (- y last-y))
	 (xdiff (- x last-x)))
    (loop for i from 0 below (abs yfloordiff)
       for mult = 2 then 1
       for until-crossing = (+ i (if pos? (- 1 (mod last-y 1)) (mod last-y 1)))
       for test = (= 0 until-crossing)
       for new-x = (+ last-x (abs (* (/ until-crossing ydiff) xdiff)))
       unless test collect new-x unless test collect (if pos? .9999999 0)
       collect (+ new-x (* .00001 mult)) collect (if pos? 0 .9999999))))
       
(defun env-mod (envelope)
  (unless (listp envelope)
    (error "envelope in env-mod should be a list but is ~a" envelope))
  (flatten
   (loop for x in envelope by #'cddr and y in (cdr envelope) by #'cddr
      with last-x = (first envelope) with last-y = (second envelope)
      for fy1 = (floor last-y) for fy2 = (floor y)
      when (and (= 0 (mod y 1)) (not (= 0 y))) do (setf y (- y .0000001))
      when (< x last-x)
      do (warn "env-mod encountered am envelope with decreasing x-value")
      unless (= fy1 fy2) collect (env-mod-aux last-x last-y x y)
      collect x collect (mod y 1)
      do (setf last-x x last-y y))))

;; *** generate-automation-data
;;; parse the envelope information from a spatial-sndfile into reaper automation
;;; data
(defmethod generate-automation-data ((ssf spatial-sndfile))
  (let* ((start (if *spatial-reaper-use-starts* (start-time ssf) 0))
	 (dur0 *spatial-reaper-duration*)
	 (dur (if dur0
		  (if (< dur0 0) (- (abs dur0) start) dur0)
		  (if *spatial-reaper-use-starts*
		      (snd-duration ssf)
		      (+ (snd-duration ssf) (start-time ssf)))))
	 ;(or *spatial-reaper-duration* (snd-duration ssf)))
	 (len (rational (* dur (/ *spatial-reaper-tempo* 60))))
	 (angle (env-mod (angle-env ssf)))
	 (elevation (env-mod (elevation-env ssf)))
	 (angle-len (loop for x in angle by #'cddr finally (return x)))
	 (elevation-len (loop for x in elevation by #'cddr finally (return x)))
	 angle-points elevation-points)
    (setf angle-points
	  (loop for x in angle by #'cddr and y in (cdr angle) by #'cddr
	     collect
	       (format nil "PT ~a ~a 0 0"
		       (float (+ (* (/ x angle-len) len) start))
		       (mod y 1.0))))
    (setf elevation-points
	  (loop for x in elevation by #'cddr and y in (cdr elevation) by #'cddr
	     collect
	       (format nil "PT ~a ~a 0 0"
		       (float (+ (* (/ x elevation-len) len) start))
		       (mod y 1.0))))
    (values angle-points elevation-points)))

;; *** write-automation-envelope
;;; return the automation envelope as string
(defmethod write-automation-envelope ((ssf spatial-sndfile) &optional stream)
  (multiple-value-bind (angle elevation)
      (generate-automation-data ssf)
    (format stream (astring ssf) angle elevation)))

;; ** spatialize
;;; convert envelopes to automation curves
;;; insert automation envelopes into project file

;; *** insert-envelopes
;;; get list of automation-pairs and insert it into the first mixer track
;;; that doesn't have an envelope yet.
(defun insert-envelopes (string spatial-sndfile)
  (let* ((scan (ppcre:create-scanner (format nil "FXID ~a.{2,8}?WAK "
					     *encoder-fxid*)
				     :single-line-mode t))
	 (aut-env (write-automation-envelope spatial-sndfile)))
    (ppcre:regex-replace scan string
			 (format nil "FXID ~a~&~a      WAK "
				 *encoder-fxid*
				 aut-env))))

;; *** write-spatial-reaper-file
;;; spatial-sndfiles - list of spatial-sndfile objects.
;;; start-times - list of start-times for each file on each track. If the list
;;;  is shorter than the number of soundfiles, it's rolled over. 
;;; file - the path to the file that's generated.
;;; use-start-times - if nil, all envelopes start at 0. If t, the envelopes use
;;;  the start-time of the respective spatial-sndfile.
;;; use-end-times - if nil, all envelopes end when every sndfile has stopped.
;;;  If t, end when the respective sndfile stopps.
;;; duration - duration of each envelope. If nil, see behavior of use-end-times.
;;;  If t, use-end-times is overwritten by this and every envelope gets the same
;;;  duration.
;;; ambi-order - should be an integer between 1 and 8, this determines the
;;;  number of channel for each track.
;;; samplerate - samplerate of the file.
;;; tempo - tempo of the file
;;; init-vol - Volume multiplier the faders will start at. Init is -12db (~.25)
(defun write-spatial-reaper-file
    (spatial-sndfiles &key file
			use-start-times
			use-end-times
			duration
			(ambi-order 3)
			(samplerate 48000)
			(tempo 60)
			(init-vol .2511)
			(encoder *iem-stereo-encoder*)
			(decoder *blue-ripple-decoder*))
  ;; sanity checks:
  (unless (and spatial-sndfiles (listp spatial-sndfiles))
    (error "spatial-sndfiles is not a list but ~a" spatial-sndfiles))
  (unless (and (integerp ambi-order) (< 0 ambi-order 9))
    (error "ambi-order should be an integer between 1 and 8 but is ~a"
	   ambi-order))
  ;; when no duration is given and we don't use end-times, look for the file
  ;; that is playing the longest
  (unless (or duration use-end-times)
    (setf duration (- (loop for snd in spatial-sndfiles and i from 0
			 maximize (+ (start-time snd) (snd-duration snd))))))
  ;; use globals to give information to #'generate-automation-data
  (setf *spatial-reaper-tempo* tempo
	*spatial-reaper-duration* duration
	*spatial-reaper-use-starts* use-start-times)
  (let* ((filename (or file (format nil "~a~a" *src-dir* "spatial-test.rpp")))
	 (channel-nr (expt (1+ ambi-order) 2))
	 (soundfiles '())
	 (start-times '())
	 string)
    (multiple-value-bind (sndfls stimes)
	(loop for snd in spatial-sndfiles
	   unless (equal 'spatial-sndfile (type-of snd))
	   do (error "~a is not of type spatial-sndfile" snd)
	   when (> (channels snd) 2)
	   do (warn "soundfiles with ~a channels detected, but ~
                                  stereo-encoders are used"
		    (channels snd))
	   collect (path snd) into sndfls
	   collect (start-time snd) into stimes
	   finally (return (values sndfls stimes)))
      (setf soundfiles sndfls start-times stimes))
    ;; write an "ordinary" reaper file first
    (write-reaper-file
     (make-spatial-reaper-file soundfiles
			       :start-times start-times
			       :samplerate samplerate)
     :file filename)
    (setf string (read-file filename)
    ;; now modify it
	 ; string (set-samplerate string samplerate)
	  string (set-master-channels string channel-nr)
	  string (set-track-channels string channel-nr)
	  string (insert-plugin-master string decoder)
	  string (insert-plugin-all-tracks string encoder)
	  string (set-all-faders string init-vol))
    ;; create all envelopes
    (loop for snd in spatial-sndfiles do
	 (setf string (insert-envelopes string snd)))
    (with-open-file 
        (out filename :direction :output :if-exists :rename-and-delete)
      (princ string out))
    (format t "~&succesfully wrote ~a" filename)))

;; ** EXAMPLE

#|
(write-spatial-reaper-file
 `(,(make-spatial-sndfile (format nil "~a~a" *src-dir* "intro.wav")
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-spatial-sndfile (format nil "~a~a" *src-dir* "continuo.wav")
			  :angle-env '(0 0  .5 .5  .8 8  1 3.25)
			  :elevation-env '(0 0.5  1 .5)
			  :start-time 10)
    ,(make-spatial-sndfile (format nil "~a~a" *src-dir* "continuo2.wav")
			  :angle-env '(0 .5  .5 1  .8 8.5  1 3.75)
			  :elevation-env '(0 0.5  1 .5)))
 :ambi-order 3
 :use-start-times t
 :use-end-times t
 :duration 10)
|#

;; EOF generate-spatial-rf.lsp
