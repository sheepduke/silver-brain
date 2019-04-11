(in-package silver-brain)

(caveman2:clear-routing-rules *server*)

(defroute ("/concepts" :method :get) (&key _parsed)
  (let ((search (assoc-value _parsed "search" :test #'string=)))
    (render-json-array
     (-<>> (if search (find-concepts-by-name search) (get-all-concepts))
       (mapcar #'concept-summary <>)))))

(defroute ("/concepts" :method :post) ()
  (match (decode-request-json-alist '(:name :content :content-format))
    (nil (throw-code 400))
    ((list name content content-format)
     (let ((concept (add-concept name content content-format)))
       (set-response-location-header
        (format nil "/concepts/~a" (concept-uuid concept)))
       (set-response-status 201)
       nil))))

(defroute ("/concepts/:id" :method :get) (&key id)
  (let ((concept (get-concept-by-id id)))
    (or concept (throw-code 404))
    (render-json
     `((:uuid . ,(concept-uuid concept))
       (:name . ,(concept-name concept))
       (:content . ,(concept-content concept))
       (:content-format . ,(concept-content-format concept))))))

(defroute ("/concepts/:id" :method :put) (&key id)
  (let ((concept (get-concept-by-id id)))
    (or concept (throw-code 404))
    (match (decode-request-json-alist '(:name :content :content-format))
      (nil (throw-code 400))
      ((list name content content-format)
       (setf (concept-name concept) name)
       (setf (concept-content concept) content)
       (setf (concept-content-format concept) content-format)))))

(defroute ("/concepts/:id" :method :delete) (&key id)
  (let ((concept (get-concept-by-id id)))
    (or concept (throw-code 404))
    (delete-concept-by-id id)
    nil))

(defroute ("/concepts/:id/parents" :method :get) (&key id)
  (let ((concept (get-concept-by-id id)))
    (or concept (throw-code 404))
    (render-json-array
     (mapcar #'concept-summary
             (get-concept-parents concept)))))

(defroute ("/concepts/:uuid/children" :method :get) (&key uuid)
  (let ((concept (get-concept-by-id uuid)))
    (or concept (throw-code 404))
    (render-json-array
     (mapcar #'concept-summary
             (get-concept-children concept)))))

(defroute ("/concepts/:uuid/friends" :method :get) (&key uuid)
  (let ((concept (get-concept-by-id uuid)))
    (or concept (throw-code 404))
    (render-json-array
     (mapcar #'concept-summary
             (get-concept-friends concept)))))
