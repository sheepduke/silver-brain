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

(define-condition bad-request (error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Server                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (and *server*
       (print "Server is not nil"))
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
  (or *server*
      (print "Server is already nil"))
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Utility                            ;;;;
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

(defmacro with-database-header (&body body)
  (with-gensyms (g-database-name)
    `(let ((,g-database-name (gethash "database"
                                      (lack.request:request-headers
                                       ningle:*request*))))
      (if (null ,g-database-name)
          (progn (set-response-code 400)
                 "Database header not specified")
          (let ((store:*database* ,g-database-name))
            ,@body)))))

(defun handle-request (fun)
  (handler-case (match (funcall fun)
                  ((list :error :not-found)
                   (set-response-code 404))
                  ((list :error :bad-request reason)
                   (set-response-code 400)
                   (format nil "~a" reason))
                  ((list :ok) "")
                  ((list :ok obj)
                   (if (stringp obj)
                       obj
                       (jsown:to-json obj))))
    (store:database-not-found-error (err)
      (set-response-code 410)
      (format nil
              "Database not found: ~a"
              (store:database-name err)))
    (bad-request ()
      (set-response-code 400)
      "")))

(defun get-query-param (key)
  (assoc-value (lack.request:request-query-parameters ningle:*request*)
               key
               :test #'string-equal))

(defun get-json-params ()
  (let ((line (read-line (lack.request:request-raw-body ningle:*request*))))
    (handler-case (jsown:parse line)
      (error () (error 'bad-request)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Router                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route *router* "/api/" :get ()
  '(:ok ""))

(define-route *router* "/api/database" :post ()
  (let* ((json (get-json-params))
         (name (jsown:val-safe json "name")))
    (concept-map:create-database name)))

(define-route *router* "/api/concept/:uuid" :get (uuid)
  (with-database-header
    (concept-map:get-concept uuid)))

(define-route *router* "/api/concept" :get ()
  (with-database-header
    (match (get-query-param "search")
      (nil '(:error :bad-request :empty-search))
      ((and (type string) search) (concept-map:search-concept search)))))

;; (print (dex:get "http://localhost:5001/api/concepts?search=soft" :headers '(("Database" . "/home/sheep/temp/a.sqlite"))))

;; (silver-brain::start)
;; (dex:get
;;  (format nil "http://localhost:5001/api/concepts/~a" "5BAAB06F-D70D-4405-8511-3032D12448B3")
;;  :headers '(("Database" . "/home/sheep/temp/a.sqlite"))
;;  )
;; (ql:quickload :dexador)
