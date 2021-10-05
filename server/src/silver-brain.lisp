(defpackage silver-brain
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:export #:main))

(in-package silver-brain)

(defun start ()
  (setf (config:active-profile) :dev)
  (silver-brain.store:start)
  (silver-brain.concept-map.cache:start)
  (silver-brain.web:start))

;; (setf (config:active-profile) :dev)
;; (silver-brain::start)
