;; * SPATIAL
;;; spatialize the generated sndfiles using slippery chicken and reaper:

(in-package :fb)

;; ** spatialize

(write-reaper-ambisonics-file
 `(,(make-sndfile "/E/code/feedback/intro.wav"
		  :angle-env '((0 0  1 7) (0 .5  1 .5))
		  :elevation-env '(0 0  1 0))
    #+nil,(make-sndfile "/E/code/feedback/intro_surround.wav"
			:angle-env '((0 .25  1 .25) (0 .75  1 .75))
			:elevation-env '(0 0  1 0))
    ,(make-sndfile "/E/code/feedback/continuo.wav"
		   :angle-env '((0 .25  1 .25) (0 .75  1 .75))
		   :elevation-env '(0 0  1 0))
    ,(make-sndfile "/E/code/feedback/continuo2.wav"
		   :angle-env '((0 0  1 0))
		   :elevation-env '((0 0  1 0) (0 1  1 1)))
    ,(make-sndfile "/E/code/feedback/continuo_surround.wav"
		   :angle-env '((0 .25  1 .25) (0 .75  1 .75))
		   :elevation-env '(0 0  1 0))
    ,(make-sndfile "/E/code/feedback/continuo2_surround.wav"
		   :angle-env '((0 0  1 0))
		   :elevation-env '((0 0  1 0) (0 1  1 1)))
    ,(make-sndfile "/E/code/feedback/intro-noise.wav"
		   :angle-env '((0 0  .7 0  1 .4) (0 .5  .7 .5  1 .6))
		   :elevation-env '(0 0  .7 .4  1 .5))
    ,(make-sndfile "/E/code/feedback/bridge.wav"
		   :angle-env '((0 0  1 0) (0 .5  1 .5))
		   :elevation-env '(0 .8  1 .8))
    #|,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/code/feedback/intro.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
    :elevation-env '(0 0  .6 .5  2 .5)))|#)
 :file "/E/feed_it/spaaaace.rpp"
 :envs-use-start-times t
 :envs-use-end-times t
 :encoder :iem-multi-encoder
 :decoder :iem-simple-decoder
 :angle-parameter-slot '(7 12)
 :elevation-parameter-slot '(8 13)
 :ambi-order 3
 :init-volume .4
 :envs-only nil)

#+nil(edit-file "/E/feed_it/spaaaace.rpp" string
  (let* ((scan (ppcre:create-scanner
		(format nil "      >~&    >~&  >~&  <EXTENSIONS~&  >~&>")
		:single-line-mode t)))
    (ppcre:regex-replace scan
t			 string
(read-file-as-string "/E/feed_it/Rendered/new.txt"))))

;; EOF spatial.lsp
