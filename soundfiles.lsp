;; * Soundfiles

(in-package :feedback)

;; ** quiet-atoms

(defparameter *quiet-atoms* (make-stored-file-list 'quiet-atoms nil))
(defparameter *quiet-atoms-txt* "/E/code/feedback/quiet-atoms.txt")
(unless (probe-file *quiet-atoms-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *quiet-atoms*
   "/E/Keks_Feedback/samples/pure_quiet/atoms/"
   :analyse t
   :auto-map nil
   :auto-scale-mapping nil
   :remap nil
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (ly::dominant-frequency sf)
				    (ly::centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (ly::transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (ly::smoothness sf)
				 0.5))))
  (ly::store-in-text-file *quiet-atoms* *quiet-atoms-txt*))

(unless *re-analyse-soundfiles*
  (setf *quiet-atoms* (ly::load-from-file *quiet-atoms-txt*)))

;; ** pure-atoms

(defparameter *pure-atoms* (make-stored-file-list 'pure-atoms nil))
(defparameter *pure-atoms-txt* "/E/code/feedback/pure-atoms.txt")
(unless (probe-file *pure-atoms-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *pure-atoms*
   "/E/Keks_Feedback/samples/pure/atoms/"
   :analyse t
   :auto-map nil
   :auto-scale-mapping nil
   :remap nil
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (ly::dominant-frequency sf)
				    (ly::centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (ly::transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (ly::smoothness sf)
				 0.5))))
  (ly::store-in-text-file *pure-atoms* *pure-atoms-txt*))

(unless *re-analyse-soundfiles*
  (setf *pure-atoms* (ly::load-from-file *pure-atoms-txt*)))

;; ** distorted

(defparameter *distorted* (make-stored-file-list 'distorted nil))
(defparameter *distorted-txt* "/E/code/feedback/distorted.txt")
(unless (probe-file *distorted-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *distorted*
   "/E/Keks_Feedback/samples/distorted/"
   :analyse t
   :auto-map nil
   :auto-scale-mapping nil
   :remap nil
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (ly::dominant-frequency sf)
				    (ly::centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (ly::transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (ly::smoothness sf)
				 0.5))))
  (ly::store-in-text-file *distorted* *distorted-txt*))

(unless *re-analyse-soundfiles*
  (setf *distorted* (ly::load-from-file *distorted-txt*)))

;; ** noise
(defparameter *noise* (make-stored-file-list 'noise nil))
(defparameter *noise-txt* "/E/code/feedback/noise.txt")
(unless (probe-file *noise-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *noise*
   "/E/Keks_Feedback/samples/noise/"
   :auto-map t
   :auto-scale-mapping t
   :remap t
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (ly::dominant-frequency sf)
				    (ly::centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (ly::transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (ly::smoothness sf)
				 0.5))))
  (ly::store-in-text-file *noise* *noise-txt*))

(unless *re-analyse-soundfiles*
  (setf *noise* (ly::load-from-file *noise-txt*)))

;; ** percussive
(defparameter *percussive* (make-stored-file-list 'percussive nil))
(defparameter *percussive-txt* "/E/code/feedback/percussive.txt")
(unless (probe-file *percussive-txt*) (setf *re-analyse-soundfiles* t))
(when *re-analyse-soundfiles*
  (folder-to-stored-file-list
   *percussive*
   "/E/Keks_Feedback/samples/percussive/polished/"
   :auto-map t
   :auto-scale-mapping t
   :remap t
   ;;:fft-size 4096
   :f1 #'(lambda (sf) (/ (log (/ (+ (ly::dominant-frequency sf)
				    (ly::centroid sf))
				 2))
			 12000))
   :f2 #'(lambda (sf) (* (expt (ly::transient sf) 0.7)
			 0.6))
   :f3 #'(lambda (sf) (- 1 (expt (ly::smoothness sf)
				 0.5))))
  (ly::store-in-text-file *percussive* *percussive-txt*))

(unless *re-analyse-soundfiles*
  (setf *percussive* (ly::load-from-file *percussive-txt*)))

(setf *re-analyse-soundfiles* nil)

;; EOF soundfiles.lsp
