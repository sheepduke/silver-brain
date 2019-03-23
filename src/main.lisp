(defpackage silver-brain
  (:use :cl))
(in-package :silver-brain)

;; blah blah blah.

;; (defvar *app* (make-instance 'ningle:app))

;; (setf (ningle:route *app* "/")
;;       "Welcome to ningle!")

;; (setf (ningle:route *app* "/change")
;;       (lambda (params)
;;         (sleep 5)))

;; (setf (ningle:route *app* "/login" :method :POST)
;;       #'(lambda (params)
;;           (if (authorize (cdr (assoc "username" params :test #'string=))
;;                          (cdr (assoc "password" params :test #'string=)))
;;               "Authorized!"
;;               "Failed...Try again.")))

;; (clack:clackup *app*)
