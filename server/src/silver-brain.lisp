(defpackage silver-brain
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:export #:main))

(in-package silver-brain)

(defun start ()
  (setf (config:active-profile) :dev)
  (silver-brain.store:start)
  (silver-brain.concept-map:start)
  (silver-brain.web:start)
  nil)

(defun stop ()
  (silver-brain.store:stop)
  (silver-brain.concept-map:stop)
  (silver-brain.web:stop)
  nil)

