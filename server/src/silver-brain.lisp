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

(defun start-dev ()
  (setf (config:active-profile) :dev)
  (migrate-all-databases)
  (silver-brain.concept-map:start)
  (silver-brain.web:start)
  nil)

(defun stop ()
  (silver-brain.concept-map:stop)
  (silver-brain.web:stop)
  nil)

(defun migrate-all-databases ()
  "Run migrations for all the database file under data dir."
  (loop for file in (uiop:directory-files (config:data-dir))
        when (string= "sqlite" (pathname-type file))
          do (silver-brain.store:with-database ((format nil "~a" file)
                                                :auto-migrate t
                                                :expand-path-p nil))))
