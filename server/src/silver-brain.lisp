(defpackage silver-brain
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:export #:main
           #:start
           #:stop))

(in-package silver-brain)

(defun start ()
  (migrate-all-databases)
  (silver-brain.concept-map:start)
  (silver-brain.web:start)
  nil)

(defun stop ()
  (silver-brain.concept-map:stop)
  (silver-brain.web:stop)
  nil)

(defun migrate-all-databases ()
  ;; TODO
  )
