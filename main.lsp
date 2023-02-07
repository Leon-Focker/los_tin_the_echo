(in-package :sc)

;; structures:
(load (cl::os-path "/E/code/layers/src/all.lsp"))
;(import 'layers::make-structure)

;; morph and interpolate patterns
(load (cl::os-path "/E/code/feedback/morph.lsp"))

;; Michael Edwards samp1 instrument
(load (compile-file "/E/code/feedback/samp1.ins"))
;; some more clm instruments:
(in-package :clm)
(load (compile-file "/home/leon/lisp/clm-6/addsnd.ins"))

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
	  ly::visualize
	  ly::visualize-structure
	  ly::decide-for-snd-file
	  ly::get-sub-list-of-closest
	  ly::folder-to-stored-file-list
	  clm::with-sound
	  clm::with-mix
	  clm::sound-let
	  clm::mix
	  clm::*CLM-MIX-CALLS*
	  clm::*CLM-MIX-OPTIONS*
	  clm::add-sound
	  clm::samp1))

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
						'((1 1)
						  (2 1.5)
						  (3 0.2)
						  (4 1.3)
						  (5 1)
						  (6 0.2)
						  (7 0.95))
						:id 'form
						:type 'compartmentalise
						:duration (* 60 7)
						:smallest 2))
			    (form (cdr (reverse (ly::data st)))))
		       ;;(ly::visualize-structure st 1 "/E/code/feedback/structure_comp1.png")
		       form))

;; load soundfiles and patterns
(load "/E/code/feedback/soundfiles.lsp")
(load "/E/code/feedback/patterns.lsp")
(load "/E/code/feedback/utilities.lsp")

;; SCORE
;;; add with-mix
;;; autoc 	pitch estimation (Bret Battey) 	autoc.ins ???
(with-sound (:header-type clm::mus-riff :sampling-rate 48000
			  :output "/E/code/feedback/test.wav"
			  :channels 2 :play nil)
;;; start-times are start-times of sections in *form*
  (let* ((start-times (append '(0) (loop for i in (first *form*) sum i into sum collect sum)))
	 (dur2 (- (startn 2) (startn 1)))
	 (rhythm1 (morph-patterns (list *pattern4* *pattern1*)
				  dur2 nil (fibonacci-transition dur2)))
	 (rhythm2 (interpolate-patterns (list *pattern4* *pattern1*)
					dur2))
	 (rhythm3 (morph-patterns (list *pattern4* *pattern1*)
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
		  		   :amp 0.7
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
				    :amp (* (- 0.99 (* line 0.5)) 0.7)))
		  #'(lambda () (samp1 file time
				      :duration duration
				      :degree degree
				      :srt 2
				      :amp (* line 0.55))))
	       :rhythm-list rhythm1
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :amp 0.7
				    :duration duration
				    :degree 0)))
	       :rhythm-list rhythm2
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5))))
      (fb-play (startn 1) (startn 2)
	       (#'(lambda () (samp1 file time
				    :amp 0.7
				    :duration duration
				    :degree 90)))
	       :rhythm-list rhythm3
	       :new-id
	       #'(lambda () (decide-for-snd-file
			     (get-sub-list-of-closest *percussive* (vector line line 0.5)
						      :max-distance 0.6)
			     (random 0.5)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ))

;; EOF main.lsp
