(defpackage silver-brain.web
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store)
                    (#:concept-map #:silver-brain.concept-map))
  (:import-from #:alexandria
                #:if-let
                #:assoc-value
                #:make-keyword
                #:with-gensyms)
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:op
                #:~>>)
  (:export #:start
           #:stop))

(in-package silver-brain.web)

(defvar *router* (make-instance 'ningle:app))

(defvar *server* nil)

(define-condition client-error (error)
  ((reason :type string :accessor reason :initarg :reason)))

(define-condition bad-request-error (client-error) ())

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
  (handler-case (let ((line (read-line (lack.request:request-raw-body
                                        ningle:*request*))))
                  (log:debug "JSON string: ~a" line)
                  (jsown:parse line))
    (error () (error 'bad-request-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Request & Response                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-service-response (response)
  (match response
    ((type string) response)
    
    ((list (and (type number) status)
           (and (type string) response))
     (list status nil (flex:string-to-octets response)))
    
    ((list :error :not-found)
     (list 404 nil nil))
    
    ((list :error :bad-request reason)
     (list 400 nil (flex:string-to-octets (format nil "~a" reason))))
    
    ((list :ok) "")
    
    ((list :ok obj) (jsown:to-json obj))))

(defgeneric send-client-error-response (err))

(defmethod send-client-error-response ((err client-error))
  (list 400
        nil
        (flex:string-to-octets (reason err))))

(defmethod send-client-error-response ((err store:database-not-found-error))
  (list 400
        nil
        (flex:string-to-octets
         (format nil "Database not found: ~a" (store:database-name err)))))

(defmacro with-request-handler ((&key (require-database nil))
                                &body body)
  `(handler-case (let ((store:*database* ,(if require-database
                                              `(get-database-name)
                                              nil)))
                   (parse-service-response (progn ,@body)))
     (bad-request-error (err) (send-client-error-response err))
     (store:database-not-found-error (err) (send-client-error-response err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Router                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-route (uri param-var
                        (&key (method :get)
                           (require-database nil))
                        &body body)
  `(setf (ningle:route *router* ,uri :method ,method)
         (lambda (,param-var)
           (declare (ignorable ,param-var))
           (with-request-handler (:require-database ,require-database)
             ,@body))))

(define-route "/api/" _ () nil)

(define-route "/api" _ () nil)

(define-route "/api/database" _ (:method :post)
  (let* ((json (get-request-body-as-json))
         (name (jsown:val-safe json "name")))
    (concept-map:create-database name)))

(define-route "/api/concept/:uuid" params (:require-database t)
  (with-path-vars (uuid) params
    (concept-map:get-concept uuid)))

(define-route "/api/concept" params (:method :post :require-database t)
  (let ((json (get-request-body-as-json)))
    (log:debug "Input JSON: ~a" json)
    (let ((uuid (concept-map:create-concept
                 :name (jsown:val-safe json "name")
                 :content-type (jsown:val-safe json "content-type")
                 :content (jsown:val-safe json "content"))))
      uuid)))

(define-route "/api/concept/:uuid" params
    (:method :patch :require-database t)
  (with-path-vars (uuid) params
    (let ((json (get-request-body-as-json)))
      (log:debug "Input JSON: ~a" json)
      (concept-map:update-concept
       uuid
       :name (jsown:val-safe json "name")
       :content-type (jsown:val-safe json "content-type")
       :content (jsown:val-safe json "content")))))

(define-route "/api/concept/:uuid" params
    (:method :delete :require-database t)
  (with-path-vars (uuid) params
    (concept-map:delete-concept uuid)))

(define-route "/api/concept" params (:require-database t)
  (let ((search-string (get-query-param "search")))
    (log:debug "Search string: ~a" search-string)
    (concept-map:search-concept search-string)))

(define-route "/api/concept-link" params (:require-database t)
  (let ((source (get-query-param "source" :default nil))
        (target (get-query-param "target" :default nil)))
    (concept-map:get-links :source source :target target)))

;; (dex:get "http://localhost:5001/api/concept?search=soft" :headers '(("Database" . "/home/sheep/temp/a.sqlite")))

;; (progn (setf (silver-brain.config:active-profile) :dev)
;;        (silver-brain:start))

;; (dex:get "http://localhost:5001/api/concept/x5BAAB06F-D70D-4405-8511-3032D12448B3" :headers '(("Database" . "a.sqlite")))

;; (dex:get "http://localhost:5001/api/concept-link?source=5BAAB06F-D70D-4405-8511-3032D12448B3" :headers '(("Database" . "a.sqlite")))
