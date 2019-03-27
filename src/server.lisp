(defpackage silver-brain.server
  (:use #:cl
        #:alexandria)
  (:export #:start
           #:stop))
(in-package silver-brain.server)

(defvar *app* (make-instance 'ningle:app))
(defvar *server* nil)

(defvar *concept-map* (make-instance 'concept-map:concept-map))

(setf (ningle:route *app* "/")
      "Welcome to ningle!")

;; (setf (ningle:route *app* "/login" :method :POST)
;;       #'(lambda (params)
;;           (if (authorize (cdr (assoc "username" params :test #'string=))
;;                          (cdr (assoc "password" params :test #'string=)))
;;               "Authorized!"
;;               "Failed...Try again.")))

(defmacro defroute (url method &rest body)
  (with-gensyms (params-name)
    `(setf (ningle:route *app* ,url :method ,method)
           (lambda (,params-name)
             (flet ((get-param (key)
                      (assoc-value ,params-name key)))
               ,@body)))))

(defroute "/concepts" :get
  (format nil "All concepts"))

(defroute "/concepts/:id" :get
  (format nil "ID: ~a" (get-param :id)))

(dexador:get "http://localhost:5000/concepts/100")

(let (((intern "ID")) 100)
  id)


;; (setf (ningle:route *app* "/concepts" :method :get)
;;       (lambda (params)
;;         (declare (ignore params))
;;         "True"))

;; (setf (ningle:route *app* "/concept/:id/children/:child-id" :method :get)
;;       (lambda (params)
;;         (format nil "I want ID: ~a" (alexandria:assoc-value params :id))))


(defun start ()
  "Start the server."
  (setf *server* (clack:clackup *app*)))

(defun stop ()
  "Stop the server."
  (clack:stop *server*)
  (setf *server* nil))

;; (start)

