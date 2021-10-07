(defpackage silver-brain.web
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store)
                    (#:concept-map #:silver-brain.concept-map))
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
  (with-gensyms (g-params)
    `(setf (ningle:route ,router ,url :method ,http-method)
           (lambda (,g-params)
             (declare (ignorable ,g-params))
             (let ,(mapcar (lambda (var)
                             `(,var (assoc-value ,g-params (make-keyword ',var))))
                    var-list)
               (handle-request (lambda () ,@body)))))))

(defun handle-request (fun)
  (let ((database-name (gethash "database"
                                (lack.request:request-headers ningle:*request*))))
    (if (null database-name)
        (progn (set-response-code 400)
               "Database header not specified")
        (handler-case (match (store:with-database (database-name)
                               (funcall fun))
                        ((list :error :not-found)
                         (set-response-code 404))
                        ((list :error :bad-request reason)
                         (set-response-code 400)
                         (format nil "~a" reason))
                        ((list :ok) "")
                        ((list :ok obj) (jsown:to-json obj)))
          (store:database-not-found-error (err)
            (set-response-code 410)
            (format nil
                    "Database not found: ~a"
                    (store:database-name err)))))))

(define-route *router* "/api/concepts/:uuid" :get (uuid)
  (concept-map:get-concept uuid))

;; (silver-brain::start)
;; (dex:get
;;  (format nil "http://localhost:5001/api/concepts/~a" "5BAAB06F-D70D-4405-8511-3032D12448B3")
;;  :headers '(("Database" . "/home/sheep/temp/a.sqlite"))
;;  )
