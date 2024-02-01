;; * los tin the echo

;;; This is the score to the algorithmically composed piece of music "los tin the echo".
;;; I use Lisp and the Common Lisp Music, Slippery Chicken and Layers Package.
;;; Additionally to this a sample library is needed and the paths to the samples
;;; must be changed in soundfiles.lsp. 

;;; CLM and Slippery Chicken and Layers must already be installed.
;;; Then copy the source code and run this file.

;;; If you have any questions contact me at contact@leonfocker.de

;; ** dependencies

(ql:quickload 'layers)
;; (alternatively you can just load 'layers manually)

(in-package :layers)

(defparameter *fb-src-dir-pathname*
  (let ((load-name (or *load-truename* *compile-file-truename*)))
    (make-pathname :directory (pathname-directory load-name)
		   :device (pathname-device load-name))))

(defparameter *fb-src-dir* (namestring *fb-src-dir-pathname*))

(import '(clm::with-sound
	  clm::with-mix
	  clm::sound-let
	  ;;clm::mix
	  clm::*CLM-MIX-CALLS*
	  clm::*CLM-MIX-OPTIONS*
	  clm::add-sound
	  ;;clm::samp0
	  ))

;; ** globals

;; set this to true, if you want to re-analyse all soundfiles, even if their
;; analysis data was saved to a text-file.
(defparameter *re-analyse-soundfiles* nil)

;; ** load

;; load soundfiles and patterns
(dolist (file '("soundfiles.lsp"
		"score.lsp"))
  (load (probe-file (format nil "~a~a" *fb-src-dir* file))))

;; YAY :)
(format t "~&done loading!")

;; EOF all.lsp
