(defpackage silver-brain.config
  (:use #:cl)
  (:export #:*profile*
           #:server-port
           #:server-print-access-log-p
           #:data-dir))

(in-package silver-brain.config)

(chameleon:defconfig
  ;; Web server port.
  (server-port 5000 "Server port.")
  (server-print-access-log-p nil)
  ;; Database file.
  (data-dir))

(chameleon:defprofile :dev
  (server-port 5001)
  (server-print-access-log-p t)
  (data-dir (truename "~/temp/silver-brain/")))

(chameleon:defprofile :test
  (data-dir (lambda ()
              (ensure-directories-exist (merge-pathnames "silver-brain/"
                                                         (uiop:temporary-directory)))))
  (server-port (chameleon:eval-once (find-port:find-port))))
