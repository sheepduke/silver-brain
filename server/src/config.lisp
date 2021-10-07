(defpackage silver-brain.config
  (:use #:cl)
  (:export #:profiles
           #:active-profile
           #:database-file
           #:server-port
           #:server-print-access-log-p))

(in-package silver-brain.config)

(chameleon:defconfig
  ;; Web server port.
  (server-port 5000 "Server port.")
  (server-print-access-log-p nil)
  ;; Database file.
  (database-file))

(chameleon:defprofile :dev
  (server-port 5001)
  (server-print-access-log-p t)
  (database-file (truename "~/temp/a.sqlite")))
