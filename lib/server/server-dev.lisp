(in-package #:silver-brain.server)

(defconst middlewares
  (list lack.middleware.accesslog:*lack-middleware-accesslog*
        lack.middleware.backtrace:*lack-middleware-backtrace*))

(def web-dev-app
  (jingle:make-app :middlewares middlewares
                   :address "127.0.0.1"
                   :port 5050
                   :debug-mode t
                   :silent-mode nil
                   :use-thread t))

(defun true? (params key)
  (let ((value (jingle:get-request-param params key nil)))
    (and value
         (string:= value "true" :ignore-case? t))))

(defun with-database (handler)
  (fun (params)
    (let ((database-name "silver-brain"))
      (mv-match (jingle:get-request-header "X-DatabaseName")
        (((list value) t) (setf database-name value)))
      (store:with-database database-name
        (funcall handler params)))))


(defun slot-name->json-key (slot-name)
  (let ((capitalize? nil))
    (io:with-output-to-string (stream)
      (loop for char across (string:downcase (if (string:ends-with? slot-name "?")
                                                 (format nil "is-~A" slot-name)
                                                 slot-name))
            if (char:= char #\-)
              do (setf capitalize? t)
            else
              do (io:write-char (if capitalize?
                                    (char:upcase char)
                                    char)
                                stream)
                 (setf capitalize? nil)))))

(defmethod shasht:print-json-value ((value time:timestamp) stream)
  (format stream "\"~A\"" (time:to-rfc3339-timestring value)))

(defun with-json-response (handler)
  (fun (params)
    (let ((shasht:*symbol-name-function* #'slot-name->json-key)
          (result (funcall handler params)))
      (io:with-output-to-string (stream)
        (shasht:write-json result stream)))))

(def custom-middlewares
  (list #'with-database
        #'with-json-response))

(defun apply-custom-middlewares (handler)
  (list:reduce custom-middlewares
               (fun (acc middleware)
                 (funcall middleware acc))
               :initial-value handler))

(defun get-concept (params)
  (let ((uuid (jingle:get-request-param params :uuid))
        (load-aliases? (true? params "load-aliases"))
        (load-attachments? (true? params "load-attachments"))
        (load-times? (true? params "load-times")))
    (concept-map:get-concept uuid
                             :load-aliases? load-aliases?
                             :load-attachments? load-attachments?
                             :load-times? load-times?)))

(defun register-request-handlers (app)
  ;; FIXME Change to the right dir.
  (jingle:serve-directory app "/swagger"
                          (path:join (global:store/root-path) "swagger/"))
  (jingle:redirect-route app "/" "/swagger")

  (setf (jingle:route app "/api/v2/concepts/:uuid")
        (apply-custom-middlewares #'get-concept)))

(def dev-settings
  (make-instance 'global:settings
                 :store/root-path (path:join (path:temporary-directory)
                                             "silver-brain.dev/")))

(defun start-dev-server ()
  (asdf:load-system 'silver-brain-tests.common)

  (let ((database-name "silver-brain"))
    (setf global:*settings* dev-settings)
    (store:ensure-data-hierarchy-exists)
    (os:ensure-file-deleted (global:store/database-path database-name))
    (store:with-database database-name
      (store:migrate)
      (pack:symbol-call 'silver-brain-tests.common.data.v2 'context (op))
      (register-request-handlers web-dev-app)
      (jingle:start web-dev-app))))

(defun stop-dev-server ()
  (when (jingle:http-server web-dev-app)
    (jingle:stop web-dev-app)))

;; (dex:get "http://localhost:5050/concepts/0001?load-aliases=true")
;; (dex:get "http://localhost:5050/concepts/0002?load-aliases=true")
;; (dex:get "http://localhost:5050/api/v2/concepts/0011?load-aliases=true&load-times=true")
;; (start-dev-server)
;; (stop-dev-server)
;; (dex:get "http://localhost:5050/")
;; (asdf:load-system "silver-brain.server" :force t)
;; (asdf:load-system 'dexador)
;; (asdf:test-system 'silver-brain.store)
;; (asdf:test-system 'silver-brain.concept-map)
