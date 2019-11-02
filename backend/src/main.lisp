(defpackage silver-brain/main
  (:use #:cl
        #:alexandria)
  (:import-from #:silver-brain/db)
  (:import-from #:silver-brain/config)
  (:import-from #:silver-brain/server)
  (:export #:main))

(in-package :silver-brain/main)

(defun main (&optional args)
  "Entry point of the Silver Brain server."
  (opts:define-opts
    (:name :help
     :short #\h
     :long "help"
     :description "print this help text")
    (:name :profile
     :short #\p
     :long "profile"
     :meta-var "PROFILE"
     :description "set profile to use
PROFILE is either 'product' (default) or 'dev'"
     :arg-parser (lambda (x) (make-keyword (string-upcase x)))))
  (let* ((options (opts:get-opts args))
         (help (getf options :help))
         (profile (getf options :profile :product)))
    (when help (print-help-and-quit))
    (config:set-profile profile)
    ;; When booted via command line, disable the thread usage. Otherwise the
    ;; program quits immediately.
    (setf (config:server-use-thread-p) nil)
    (db:setup)
    (server:start)))

(defun print-help-and-quit ()
  "Print help message and quit the software."
  (opts:describe :prefix "Silver Brain - Your external memory device.

Starts the server of Silver Brain software."
                 :usage-of "silver-brain")
  (uiop:quit 0))
