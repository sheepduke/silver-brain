(defpackage silver-brain
  (:use #:cl)
  (:import-from #:serapeum
                #:op)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store))
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
  (config:switch-profile :dev)
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
  (mapc (op (store:with-database (_ :auto-migrate t)))
        (silver-brain.store:list-databases)))
