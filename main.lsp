;; * FEEDBACK

(in-package :sc)

;; ** dependencies

;; structures:
(load (cl::os-path "/E/code/layers/src/all.lsp"))
;(import 'layers::make-structure)

;; morph and interpolate patterns
(load (cl::os-path "/E/code/feedback/morph.lsp"))

;; Michael Edwards samp1 instrument, but you can select the input channel:
;; it would also be possible to use samp5 (slippery chicken), but long-term
;; I would prefer a sampler that supports ambisonics output.
(load (compile-file "/E/code/feedback/samp0.ins"))

;; use all in one package
(defpackage :feedback
  (:use :cl :sc)
  (:nicknames :fb))

(in-package :feedback)

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
	  ;;clm::samp1
	  clm::samp0))

(defparameter *re-analyse-soundfiles* nil)

;; ** load

;; load soundfiles and patterns
(load "/E/code/feedback/soundfiles.lsp")
(load "/E/code/feedback/patterns.lsp")
(load "/E/code/feedback/utilities.lsp")
(load "/E/code/feedback/transitions.lsp")
(load "/E/code/feedback/generate-spatial-rf.lsp")
(load "/E/code/feedback/score.lsp")

;; EOF dependencies.lsp
