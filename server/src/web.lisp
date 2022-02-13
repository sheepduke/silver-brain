(defpackage silver-brain.web
  (:use #:cl
        #:silver-brain.util)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store)
                    (#:concept-map #:silver-brain.concept-map))
  (:import-from #:alexandria
                #:if-let
                #:assoc-value
                #:make-keyword
                #:with-gensyms)
  (:import-from #:trivia
                #:multiple-value-match)
  (:import-from #:serapeum
                #:op
                #:~>>)
  (:export #:start
           #:stop))

(in-package silver-brain.web)

(defvar *router* (make-instance 'ningle:app))

(defvar *server* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Server                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (and *server*
       (log:debug "Server is not nil"))
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
      (log:debug "Server is already nil"))
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Parameter                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-query-param (key &key (default nil default-provided-p))
  (if-let (value (assoc-value (lack.request:request-query-parameters
                               ningle:*request*)
                              key
                              :test #'string-equal))
    value
    (if default-provided-p
        default
        (error 'bad-request-error
               :reason (format nil "Query parameter ~a not found" key)))))

(defun get-path-param (key params)
  (if-let (value (assoc-value params key))
    value
    (error 'bad-request-error
           :reason (format nil "Path parameter ~a not found" key))))

(defmacro with-path-vars (vars params &body body)
  (with-gensyms (g-params)
    `(let ((,g-params ,params))
       (let ,(mapcar (lambda (var)
                       `(,var (get-path-param ,(make-keyword var)
                                                ,g-params)))
              vars)
         ,@body))))

(defun get-database-name (&key suppress-error)
  (if-let (value (gethash "database"
                          (lack.request:request-headers ningle:*request*)))
    value
    (if suppress-error
        nil
        (error 'bad-request-error
               :reason "Database name not found in HTTP header"))))

(defun get-request-body-as-json ()
  (handler-case (let ((request-body (flexi-streams:octets-to-string
                                     (lack.request:request-content ningle:*request*)
                                     :external-format :utf-8)))
                  (log:debug "Request body: ~a" request-body)
                  (jsown:parse request-body))
    (error () (error 'bad-request-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Request & Response                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-response (body &optional (status 200) headers)
  (let ((json (jsown:to-json body)))
    (log:debug json)
    (list status headers
          (flex:string-to-octets
           json))))

(defmacro with-request-handler ((&key (require-database t))
                                &body body)
  `(handler-case (let ((store:*database* ,(if require-database
                                              `(get-database-name)
                                              nil)))
                   ,@body)
     (not-found-error (err)
       (make-response (format nil "~a" err) 404))
     (bad-request-error (err)
       (make-response (format nil "~a" err) 400))
     (store:database-not-found-error (err)
       (make-response (format nil "~a" err) 400))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Router                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-route (uri param-var
                        (&key (method :get)
                           (require-database t))
                        &body body)
  `(setf (ningle:route *router* ,uri :method ,method)
         (lambda (,param-var)
           (declare (ignorable ,param-var))
           (with-request-handler (:require-database ,require-database)
             ,@body))))

(define-route "/api/" _ (:require-database nil)
  nil)

(define-route "/api" _ (:require-database nil)
  nil)

(define-route "/api/databases" _ (:method :post :require-database nil)
  (let* ((json (get-request-body-as-json))
         (name (jsown:val-safe json "name")))
    (concept-map:create-database name)
    nil))

(define-route "/api/databases" _ (:require-database nil)
  (jsown:to-json (store:list-databases)))

(define-route "/api/concepts/:uuid" params ()
  (with-path-vars (uuid) params
    (make-response (concept-map:get-concept uuid))))

(define-route "/api/concepts" params (:method :post)
  (let ((json (get-request-body-as-json)))
    (log:debug "Input JSON: ~a" json)
    (make-response
     (concept-map:create-concept
      :name (jsown:val-safe json "name")
      :content-type (jsown:val-safe json "contentType")
      :content (jsown:val-safe json "content")))))

(define-route "/api/concepts/:uuid" params
    (:method :patch)
  (with-path-vars (uuid) params
    (let ((json (get-request-body-as-json)))
      (log:debug "Input JSON: ~a" json)
      (concept-map:update-concept
       uuid
       :name (jsown:val-safe json "name")
       :content-type (jsown:val-safe json "contentType")
       :content (jsown:val-safe json "content"))))
  nil)

(define-route "/api/concepts/:uuid" params
    (:method :delete)
  (with-path-vars (uuid) params
    (concept-map:delete-concept uuid))
  nil)

(define-route "/api/concepts" params ()
  (let ((search-string (get-query-param "search")))
    (log:debug "Search string: ~a" search-string)
    (make-response
     (concept-map:search-concept search-string))))

(define-route "/api/concept-links" params (:method :post)
  (let ((json (get-request-body-as-json)))
    (log:debug "Input JSON: ~a" json)
    (make-response
     (concept-map:create-link (jsown:val json "source")
                              (jsown:val json "relation")
                              (jsown:val json "target")
                              (jsown:val json "isDirectional")))))

(define-route "/api/concept-links/:uuid" params (:method :delete)
  (with-path-vars (uuid) params
    (concept-map:delete-link uuid))
  nil)
