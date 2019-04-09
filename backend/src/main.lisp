(in-package :silver-brain)

(defun main (&optional args)
  (opts:define-opts
    (:name :profile
     :description "profile of program selected from: develop, product, testing"
     :short #\p
     :long "profile"
     :arg-parser (lambda (arg) (make-keyword (string-upcase arg))))
    (:name :help
     :description "print this help text"
     :short #\h
     :long "help"))
  (let* ((options (unix-opts:get-opts args))
         (help (getf options :help))
         (profile (getf options :profile)))
    (when help
      (print-help-and-quit))
    (cond
      ((null profile)
       (or (profile-set-p)
           (panic "Profile not set by argument or environment variable.")))
      (t
       (if (member profile (core:profiles))
           (progn (core:set-profile profile)
                  (core:setup-db))
           (panic "Profile is not valid."))))
    (server:start-server)
    (iter (while t))))

(defun profile-set-p ()
  (uiop:getenv core:*profile-env*))

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
