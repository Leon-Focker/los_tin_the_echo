;; * FEEDBACK

;; ** dependencies

;; Path to Layers Package:
(load "/E/code/layers/src/all.lsp")

(in-package :layers)

(defparameter *fb-src-dir*
  (directory-name (namestring *load-pathname*)))

#|
;; morph and interpolate patterns
;;(load (probe-file (format nil "~a~a" *fb-src-dir* "morph.lsp")))

;; Michael Edwards samp1 instrument, but you can select the input channel:
;; It would also be possible to use samp5 (slippery chicken).
(load (compile-file (probe-file (format nil "~a~a" *fb-src-dir* "samp0.ins"))))
|#

(import '(clm::with-sound
	  clm::with-mix
	  clm::sound-let
	  ;;clm::mix
	  clm::*CLM-MIX-CALLS*
	  clm::*CLM-MIX-OPTIONS*
	  clm::add-sound
	  clm::samp0))

;; ** globals

;; set this to true, if you want to re-analyse all soundfiles, even if their
;; analysis data was saved to a text-file.
(defparameter *re-analyse-soundfiles* nil)
;; for the generation of spatial audio files with reaper:
(set-sc-config 'reaper-files-for-windows t)

;; ** load

;; load soundfiles and patterns
(dolist (file '("soundfiles.lsp"
		"patterns.lsp"
		;;"score.lsp"
		))
  (load (probe-file (format nil "~a~a" *fb-src-dir* file))))

;; YAY :)
(format t "~&done loading!")

;; EOF all.lsp
