(in-package :silver-brain)

(defun main (&optional args)
  (opts:define-opts
    (:name :help
     :description "print this help text"
     :short #\h
     :long "help")
    (:name :profile
     :description "set profile to use"
     :short #\p
     :long "profile"
     :arg-parser (lambda (x) (make-keyword (string-upcase x)))))
  (let* ((options (unix-opts:get-opts args))
         (help (getf options :help))
         (profile (if-let (custom-profile (getf options :profile))
                    custom-profile
                    :product)))
    (when help (print-help-and-quit))
    (conf:set-profile profile)
    (setf (conf:server-use-thread-p) nil)
    (setup-db)
    (start-server)))

(defun panic (control-string &rest format-arguments)
  (apply #'format
         (append (list *error-output*
                       (str:concat control-string "~&"))
                 format-arguments))
  (uiop:quit 1))

(defun print-help-and-quit ()
  (opts:describe :prefix "Silver Brain - Your external memory device."
                 :suffix "
This is the long description.
"
                 :usage-of "silver-brain")
  (uiop:quit 0))
