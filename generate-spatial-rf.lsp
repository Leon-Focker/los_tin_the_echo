;; * SPATIAL
;; the purpose of this file is to automatically generate reaper files that
;; spatialize soundfiles using the IEM ambisonics plugins

;; ** general

(in-package :fb)

;; load a regex library
(ql:quickload "cl-ppcre")

;; some global variables
(defparameter *stereo-encoder-fxid*
  "{21BA0349-8E33-48D7-94BA-D648B035A6FF}")
(defparameter *blue-ripple-O3A-decoder-fxid*
  "{8898DF47-1C03-44CF-878F-50DE85568C55}")
(defparameter *spatial-reaper-tempo* 60)

;; *** read-file
;;; read an entire file a into string and return it
(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

;; ** Project
;;; first create a reaper project that contains all samples:
;;; make sure the tracks are routed correctly (mastertrack for decoder)

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
(defun make-spatial-reaper-file (soundfiles &optional (start-times '(0)))
  (let* ((items (make-reaper-items4 soundfiles start-times)))
    (make-reaper-file 'spatial items :tempo *spatial-reaper-tempo*)))

;; ** Insert changes

;; *** small adjustments

(defun set-samplerate (string &optional (samplerate 48000))
  (let* ((scan1 (ppcre:create-scanner "SAMPLERATE [0-9]+"))
	 (scan2 (ppcre:create-scanner "RENDER_FMT [0-9]+ [0-9]+ [0-9]+")))
    (setf string
	  (ppcre:regex-replace-all scan1 string
			     (format nil "SAMPLERATE ~a"
				     samplerate)))
    (ppcre:regex-replace-all scan2 string
			     (format nil "RENDER_FMT 0 2 ~a"
				     samplerate))))

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

;; *** IEM-plugins
;;; insert iem plugin on each track
;;; encoder and decoder...

;; **** insert-plugins
(defun insert-plugins (string &optional (insert "/E/code/feedback/add-iem.txt"))
  (let* ((scan (ppcre:create-scanner "MAINSEND.{6,10}?<ITEM"
	       :single-line-mode t)))
    (ppcre:regex-replace-all scan string
			     (format nil "MAINSEND 1 0 ~&~a~&<ITEM"
				     (format nil (read-file insert)
					     *stereo-encoder-fxid*)))))

;; **** insert-master-plugin
(defun insert-master-plugin (string &optional
				      (insert "/E/code/feedback/blue-ripple.txt"))
  (let* ((scan (ppcre:create-scanner "MASTER_SEL [0-9]+.{2,6}?<MASTERPLAYSPEEDENV"
				     :single-line-mode t)))
    (ppcre:regex-replace-all scan string
			     (format nil "MASTER_SEL 0~&~a~&<MASTERPLAYSPEEDENV"
				     (format nil (read-file insert)
					     *blue-ripple-O3A-decoder-fxid*)))))

;; *** faders
(defun set-all-faders (string &optional (set-to .5))
  (let* ((scan (ppcre:create-scanner "AUTOMODE ..{2,6}?VOLPAN [0-9]+?\[.]?[0-9]*"
				     :single-line-mode t)))
    (ppcre:regex-replace-all scan string
			     (format nil "AUTOMODE 0~&    VOLPAN ~a"
				     set-to))))

;;; something like plugin ID might change for every user...

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
            :initform (read-file "/E/code/feedback/astring.txt"))))

;; *** make-spatial-sndfile
;;; not meant to be called from within a sndfile-palette object, so simple init
(defun make-spatial-sndfile (path &key (angle-env '(0 .5  100 .5))
				    (elevation-env '(0 .5  100 .5))
				    id data duration end (start 0.0)
				    (frequency nil)
				    (amplitude 1.0))
  (make-instance 'spatial-sndfile
		 :id id :data data :path path :duration duration
		 :angle-env angle-env :elevation-env elevation-env
		 :frequency frequency :end end :start start
		 :amplitude amplitude))

;; *** env-mod
;;; modulo for envelope curves, using linear interpolation to create
;;; intermedeate points
(defun env-mod-aux (lastx lasty x y)
  (let* ((ydiff (- (floor y) (floor lasty)))
	 (xdiff (- x lastx)))
    (loop for i from 0 below (abs ydiff)
       for new-x = (+ lastx (abs (* (/ (+ i (- 1 (mod lasty 1))) ydiff) xdiff)))
       collect new-x collect (if (> ydiff 0) 0 1)
       collect (+ new-x .001) collect (if (> ydiff 0) 1 0))))
       
(defun env-mod (envelope)
  (unless (listp envelope)
    (error "envelope in env-mod should be a list but is ~a" envelope))
  (flatten
   (loop for x in envelope by #'cddr and y in (cdr envelope) by #'cddr
      with lastx = (first envelope) with lasty = (second envelope)
      for fy1 = (floor lasty) for fy2 = (floor y)
      unless (= fy1 fy2)
      collect (env-mod-aux lastx lasty x y)
      collect x collect (mod1 y)
      do (setf lastx x lasty y))))

;; *** generate-automation-data
;;; parse the envelope information from a spatial-sndfile into reaper automation
;;; data
(defmethod generate-automation-data ((ssf spatial-sndfile))
  (let* ((len (rational (* (snd-duration ssf) (/ *spatial-reaper-tempo* 60))))
	 (angle (env-mod (angle-env ssf)))
	 (elevation (env-mod (elevation-env ssf)))
	 (angle-len (loop for x in angle by #'cddr finally (return x)))
	 (elevation-len (loop for x in elevation by #'cddr finally (return x)))
	 angle-points elevation-points)
    (print angle)
    (setf angle-points
	  (loop for x in angle by #'cddr and y in (cdr angle) by #'cddr
	     collect
	       (format nil "PT ~a ~a 0 0"
		       (float (* (/ x angle-len) len))
		       (mod y 1.0))))
    (setf elevation-points
	  (loop for x in elevation by #'cddr and y in (cdr elevation) by #'cddr
	     collect
	       (format nil "PT ~a ~a 0 0"
		       (float (* (/ x elevation-len) len))
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
					     *stereo-encoder-fxid*)
				     :single-line-mode t))
	 (aut-env (write-automation-envelope spatial-sndfile)))
    (ppcre:regex-replace scan string
			 (format nil "FXID ~a~&~a      WAK "
				 *stereo-encoder-fxid*
				 aut-env))))

;; *** write-spatial-reaper-file

;;; spatial-sndfiles - list of spatial-sndfile objects
(defun write-spatial-reaper-file (spatial-sndfiles &key (start-times '(0))
					       (ambi-order 3)
						     file
						     (samplerate 48000)
						     (tempo 60)
						     (init-vol .2511))
  ;; sanity checks:
  (unless (and spatial-sndfiles (listp spatial-sndfiles))
    (error "spatial-sndfiles is not a list but ~a" spatial-sndfiles))
  (unless (and (integerp ambi-order) (< 0 ambi-order 9))
    (error "ambi-order should be an integer between 1 and 8 but is ~a"
	   ambi-order))
  (unless (and start-times (listp start-times))
    (setf start-times '(0)))
  (setf *spatial-reaper-tempo* tempo)
  (let* ((filename (or file "/E/code/feedback/reapertest/spatial-test.rpp"))
	 (channel-nr (expt (1+ ambi-order) 2))
	 (soundfiles (loop for snd in spatial-sndfiles
			unless (equal 'spatial-sndfile (type-of snd))
			do (error "~a is not of type spatial-sndfile" snd)
			when (> (channels snd) 2)
			do (warn "soundfiles with ~a channels detected, but ~
                                  stereo-encoders are used"
				 (channels snd))
			collect (path snd)))
	 string)
    ;; write an "ordinary" reaper file first
    (write-reaper-file
     (make-spatial-reaper-file soundfiles start-times)
     :file filename)
    (setf string (read-file filename)
    ;; now modify it
	  string (set-samplerate string samplerate)
	  string (set-master-channels string channel-nr)
	  string (set-track-channels string channel-nr)
	  string (insert-master-plugin string)
	  string (insert-plugins string)
	  string (set-all-faders string init-vol))
    ;; create all envelopes
    (loop for snd in spatial-sndfiles do
	 (setf string (insert-envelopes string snd)))
    (with-open-file 
        (out filename :direction :output :if-exists :rename-and-delete)
      (princ string out))
    (format t "~&succesfully wrote ~a" filename)))

;; ** EXAMPLE

(write-spatial-reaper-file
 `(,(make-spatial-sndfile "/E/code/feedback/glissando.wav"
			  :angle-env '(0 1  1 1.5)
			  :elevation-env '(0 1  1 2)))
 :ambi-order 3)

;; TODO
;; test if other reaper configurations work (plugin id etc), make fx-id a variable?
;; nice implementation of envelopes - env-mod
;; multichannel implementation? or stereo to mono (with auto balance)
;; env-mod isn't working properly
;; (env-mod '(0 1 1 1.5))
;; => (0 1 1 1/2)
;; EOF generate-spatial-rf.lsp
