;; * FEEDBACK

(in-package :sc)

;; ** dependencies

;; Layers Package:
(load (cl::os-path "/E/code/layers/src/all.lsp"))

;; use all in one package
(defpackage :feedback
  (:use :cl :sc)
  (:nicknames :fb))

(in-package :feedback)

(defparameter *src-dir*
  (ly::os-path (ly::directory-name (namestring *load-pathname*))))

;; morph and interpolate patterns
(load (probe-file (format nil "~a~a" *src-dir* "morph.lsp")))

;; Michael Edwards samp1 instrument, but you can select the input channel:
;; It would also be possible to use samp5 (slippery chicken).
(load (compile-file (probe-file (format nil "~a~a" *src-dir* "samp0.ins"))))

(import '(ly::make-structure
	  ly::make-stored-file
	  ly::make-stored-file-list
	  ly::store-file-in-list
	  ly::compartmentalise
	  ly::visualize-structure
	  ly::decide-for-snd-file
	  ly::get-sub-list-of-closest
	  ly::folder-to-stored-file-list
	  ly::mirrors
	  clm::with-sound
	  clm::with-mix
	  clm::sound-let
	  clm::mix
	  clm::*CLM-MIX-CALLS*
	  clm::*CLM-MIX-OPTIONS*
	  clm::add-sound
	  clm::samp0))

;; ** globals

(defparameter *re-analyse-soundfiles* nil)
;; for the generation of spatial audio files with reaper:
(set-sc-config 'reaper-files-for-windows t)
(defparameter *spatial-reaper-tempo* 60)
(defparameter *spatial-reaper-duration*  nil)

;; ** load

;; load soundfiles and patterns
(load "/E/code/feedback/soundfiles.lsp")
(load "/E/code/feedback/patterns.lsp")
(load "/E/code/feedback/utilities.lsp")
(load "/E/code/feedback/transitions.lsp")
(load "/E/code/feedback/generate-spatial-rf.lsp")
(load "/E/code/feedback/score.lsp")

(dolist (file '("soundfiles.lsp"
		"utilities.lsp"
		"patterns.lsp"
		"transitions.lsp"
		"generate-spatial-rf.lsp"
		"score.lsp"
		))
  (load (probe-file (format nil "~a~a" *src-dir* file))))

;; YAY :)
(format t "~&done loading!")

;; EOF dependencies.lsp