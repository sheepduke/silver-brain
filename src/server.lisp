(defpackage silver-brain.server
  (:nicknames server)
  (:use #:cl
        #:alexandria
        #:iterate)
  (:import-from #:cl-json
                #:encode-json-to-string)
  (:import-from #:caveman2
                #:defroute
                #:*request*)
  (:export #:server
           #:setup
           #:start
           #:stop))
(in-package silver-brain.server)

(defclass server (caveman2:<app>)
  ((handler :accessor handler
            :initform nil
            :documentation "Server that will be running."))
  (:documentation "The server."))

(defvar *server* (make-instance 'server))

(defun start (&key (port 5000) (debug nil))
  "Start the server."
  (unless (handler *server*)
    (setf (handler *server*)
          (clack:clackup *server*
                         :port port
                         :debug debug))))

(defun stop ()
  "Stop the server."
  (when (handler *server*)
    (clack:stop (handler *server*))
    (setf (handler *server*) nil)))

(defun setup (concept-map)

  (caveman2:clear-routing-rules *server*)

  (defroute ("/" :method :get) ()
      "Hello")

  (defroute ("/concepts" :method :get) ()
      (encode-json-to-string 
       (iter (for (key value) in-hashtable (concept-map:concepts concept-map))
         (collect `((:id . ,(concept:id value))
                    (:name . ,(concept:name value)))))))

  (defroute ("/concepts" :method :POST) ()
      ;; TODO
      (decode-json-from-string (request-body)))

  (defroute ("/concepts/:id" :method :get) (&key id)
      (let ((concept (concept-map:get-by-id concept-map id)))
        (encode-json-to-string
         `((:id . ,(concept:id concept))
           (:name . ,(concept:name concept))
           (:content . ,(concept:content concept))))))

  (defroute ("/concepts/:id/children" :method :get) (&key id)
      (let ((concept (concept-map:get-by-id concept-map id)))
        (encode-json-to-string
         (mapcar #'concept-summary (concept:children concept)))))

  (defroute ("/concepts/:id/parents" :method :get) (&key id)
      (let ((concept (concept-map:get-by-id concept-map id)))
        (encode-json-to-string
         (mapcar #'concept-summary (concept:parents concept))))))

(defun concept-summary (concept)
  "Return an alist representing summary information of given `concept`."
  `((:id . ,(concept:id concept))
    (:name . ,(concept:name concept))))

(defun request-body ()
  "Extract and return raw request body as string."
  (flexi-streams:octets-to-string
   (lack.request:request-content *request*) :external-format :utf-8))

(defun render-json (thing)
  )
