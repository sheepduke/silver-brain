(defpackage silver-brain.web
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:import-from #:alexandria
                #:assoc-value
                #:make-keyword
                #:with-gensyms)
  (:import-from #:trivia
                #:match)
  (:export #:start
           #:stop))

(in-package silver-brain.web)

(defvar *router* (make-instance 'ningle:app))

(defvar *server* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Server                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (unless *server*
    (setf *server*
          (clack:clackup (lack.builder:builder
                          (:static :path "/static"
                                   :root "static/")
                          (if (config:server-print-access-log-p) :accesslog nil)
                          *router*)
                         :port (config:server-port)
                         :use-thread t))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Router                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-response-code (code)
  (setf (lack.response:response-status ningle:*response*)
        (format nil "~a" code)))

(defmacro define-route (router url http-method var-list &body body)
  "Define route for given APP.
HTTP-METHOD is one of GET, POST, PUT and DELETE.
URL-RULE is the rule that will be passed to route definition.
VAR-LIST is a list of variables that will be interpreted from path variable or
request parameter."
  (check-type var-list list "lambda list")
  (with-gensyms (g-params g-router)
    `(setf (ningle:route ,router ,url :method ,http-method)
           (lambda (,g-params)
             (declare (ignorable ,g-params))
             (print ,g-params)
             (let ,(mapcar (lambda (var)
                             `(,var (assoc-value ,g-params (make-keyword ',var))))
                    var-list)
               (match (progn ,@body)
                 ((list :error :not-found)
                  (set-response-code 404))
                 ((list :error :bad-request reason)
                  (set-response-code 400)
                  (format nil "~a" reason))
                 ((list :ok) "")
                 ((list :ok obj) (jsown:to-json obj))
                 (it (format t "This should never happen")
                     (print it))))))))

(define-route *router* "/api/concepts/:uuid" :get (uuid)
  (silver-brain.concept-map.service:get-concept uuid))
