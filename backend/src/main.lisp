(in-package silver-brain)

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
     :arg-parser (lambda (x) (make-keyword (string-upcase x))))
    (:name :use-thread
     :short #\t
     :long "use-thread"
     :description "start server in another thread (debug use)"))
  (let* ((options (opts:get-opts args))
         (help (getf options :help))
         (profile (getf options :profile :product))
         (use-thread (getf options :use-thread)))
    (when help (print-help-and-quit))
    (unless use-thread
      (config:set-profile profile))
    ;; When booted via command line, disable the thread usage. Otherwise the
    ;; program quits immediately.
    (setf (config:server-use-thread-p) nil)
    (db:setup)
    (service:setup)
    (server:start)))

(defun print-help-and-quit ()
  "Print help message and quit the software."
  (opts:describe :prefix "Silver Brain - Your external memory device.

Starts the server of Silver Brain software."
                 :usage-of "silver-brain")
  (uiop:quit 0))
