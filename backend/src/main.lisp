(in-package :silver-brain)

(defun main (&optional args)
  (opts:define-opts
    (:name :help
     :description "print this help text"
     :short #\h
     :long "help"))
  (let* ((options (unix-opts:get-opts args))
         (help (getf options :help))
         (profile (getf options :profile)))
    (when help
      (print-help-and-quit))
    (set-profile :product)
    (uiop:chdir (get-config :app-root))
    (setf *default-pathname-defaults* (uiop:getcwd))
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
This is 
"
                 :usage-of "silver-brain")
  (uiop:quit 0))
