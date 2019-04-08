(in-package silver-brain.server)

(defclass server (caveman2:<app>)
  ((handler :accessor handler
            :initform nil
            :documentation "Server that will be running."))
  (:documentation "The server."))

(defvar *server* (make-instance 'server))

(defun start-server (&key (port 5000) (debug nil))
  "Start the server."
  (unless (handler *server*)
    (setf (handler *server*)
          (clack:clackup *server*
                         :port port
                         :debug debug))))

(defun stop-server ()
  "Stop the server."
  (when (handler *server*)
    (clack:stop (handler *server*))
    (setf (handler *server*) nil)))

(defun setup-server (concept-map)

  (caveman2:clear-routing-rules *server*)

  (defroute ("/concepts" :method :get) ()
    (render-json
     `((:count . ,(concept-count concept-map)))))

  (defroute ("/concepts/" :method :get) ()
    (render-json-array
     (map-concept
      concept-map
      (lambda (concept)
        `((:id . ,(concept-id concept))
          (:name . ,(concept-name concept)))))))

  (defroute ("/concepts/" :method :post) ()
    (match (decode-request-json-alist '(:name :content :content-format)
                                      :strict nil)
      (nil (throw-code 400))
      ((list name content content-format)
       (let ((concept (make-instance 'concept
                                     :name name
                                     :content-format content-format
                                     :content content)))
         (add-concept concept-map concept)
         (set-response-location-header
          (format nil "/concepts/~a" (concept-id concept)))
         (set-response-status 201)
         nil))))

  (defroute ("/concepts/:id" :method :get) (&key id)
    (let ((concept (get-concept-by-id concept-map id)))
      (or concept (throw-code 404))
      (render-json
       `((:id . ,(concept-id concept))
         (:name . ,(concept-name concept))
         (:content . ,(concept-content concept))
         (:content-format . ,(concept-content-format concept))))))

  (defroute ("/concepts/:id" :method :put) (&key id)
    (let ((concept (get-concept-by-id concept-map id)))
      (or concept (throw-code 404))
      (match (decode-request-json-alist '(:name :content))
        (nil (throw-code 400))
        ((list name content)
         (setf (concept-name concept) name)
         (setf (concept-content concept) content)))))

  (defroute ("/concepts/:id" :method :delete) (&key id)
    (let ((concept (get-concept-by-id concept-map id)))
      (or concept (throw-code 404))
      (delete-concept-by-id concept-map id)
      nil))

  (defroute ("/concepts/:id/children" :method :get) (&key id)
    (let ((concept (get-concept-by-id concept-map id)))
      (render-json
       (mapcar #'concept-summary (concept-children concept)))))

  (defroute ("/concepts/:id/parents" :method :get) (&key id)
    (let ((concept (get-concept-by-id concept-map id)))
      (render-json
       (mapcar #'concept-summary (concept-parents concept)))))

  )

(defun concept-summary (concept)
  "Return an alist representing summary information of given `concept`."
  `((:id . ,(concept-id concept))
    (:name . ,(concept-name concept))))
