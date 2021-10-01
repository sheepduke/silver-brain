(defpackage silver-brain.config
  (:use #:cl))

(in-package silver-brain.config)

(chameleon:defconfig
  ;; Web server port.
  (server-port 5000
               "Server port.")
  ;; Database file.
  (database-file))

(chameleon:defprofile :dev
  (server-port 5001)
  (database-file (truename "~/temp/a.sqlite")))
