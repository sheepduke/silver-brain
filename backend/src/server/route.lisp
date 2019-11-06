(defpackage silver-brain/server/route
  (:use #:cl
        #:alexandria
        #:cl-arrows
        #:trivia
        #:silver-brain/server/util)
  (:import-from #:silver-brain/db/concept)
  (:import-from #:silver-brain/db/concept-relation)
  (:import-from #:caveman2
                #:defroute
                #:throw-code)
  (:export #:*app*))

(in-package silver-brain/server/route)

(defvar *app*
  (make-instance 'caveman2:<app>))

(caveman2:clear-routing-rules *app*)

(defroute ("/" :method :get) ()
  #P"index.html")

(defroute ("/api/" :method :get) ()
  "Hello!")

(defroute ("/api/concepts" :method :get) (&key _parsed)
  (let ((search (assoc-value _parsed "search" :test #'string=)))
    (render-json-array
     (-<>> (if search
               (db/concept:find-by-name search)
               (db/concept:get-all))
       (mapcar #'concept-summary <>)))))

(defroute ("/api/concepts" :method :post) ()
  (match (decode-request-json-alist '(:name :content :content-format))
    (nil (throw-code 400))
    ((list name content content-format)
     (let ((concept (db/concept:insert name content content-format)))
       (set-response-location-header
        (format nil "/concepts/~a" (db/concept:uuid concept)))
       (set-response-status 201)
       nil))))

(defroute ("/api/concepts/:uuid" :method :get) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (render-json
     `((:uuid . ,(db/concept:uuid concept))
       (:name . ,(db/concept:name concept))
       (:content . ,(db/concept:content concept))
       (:content-format . ,(db/concept:content-format concept))))))

(defroute ("/api/concepts/:uuid" :method :put) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (match (decode-request-json-alist '(:name :content :content-format))
      (nil (throw-code 400))
      ((list name content content-format)
       (setf (db/concept:name concept) name)
       (setf (db/concept:content concept) content)
       (setf (db/concept:content-format concept) content-format)
       (db/concept:save concept)))))

(defroute ("/api/concepts/:uuid" :method :delete) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (db/concept:erase concept)
    nil))

(defroute ("/api/concepts/:uuid/parents" :method :get) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (render-json-array
     (mapcar #'concept-summary
             (db/concept-relation:get-parents-uuid concept)))))

(defroute ("/api/concepts/:uuid/parents/:parent-uuid" :method :put)
    (&key uuid parent-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (parent (get-concept-by-uuid-or-404 parent-uuid)))
    (db/concept-relation:become-child concept parent)
    nil))

(defroute ("/api/concepts/:uuid/parents/:parent-uuid" :method :delete)
    (&key uuid parent-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (parent (get-concept-by-uuid-or-404 parent-uuid)))
    (db/concept-relation:remove-child concept parent)
    nil))

(defroute ("/api/concepts/:uuid/children" :method :get) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (render-json-array
     (mapcar #'concept-summary
             (db/concept-relation:get-children-uuid concept)))))

(defroute ("/api/concepts/:uuid/children/:child-uuid" :method :put)
    (&key uuid child-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (child (get-concept-by-uuid-or-404 child-uuid)))
    (db/concept-relation:become-child child concept)
    nil))

(defroute ("/api/concepts/:uuid/children/:child-uuid" :method :delete) (&key uuid child-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (child (get-concept-by-uuid-or-404 child-uuid)))
    (db/concept-relation:remove-child child concept)
    nil))

(defroute ("/api/concepts/:uuid/children/:child-uuid" :method :delete) (&key uuid child-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (child (get-concept-by-uuid-or-404 child-uuid)))
    (db/concept-relation:unlink concept child)
    nil))

(defroute ("/api/concepts/:uuid/friends" :method :get) (&key uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (render-json-array
     (mapcar #'concept-summary
             (db/concept-relation:get-friends-uuid concept)))))

(defroute ("/api/concepts/:uuid/friends/:friend-uuid" :method :put) (&key uuid friend-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (friend (get-concept-by-uuid-or-404 friend-uuid)))
    (db/concept-relation:become-friend concept friend)
    nil))

(defroute ("/api/concepts/:uuid/friends/:friend-uuid" :method :delete) (&key uuid friend-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (friend (get-concept-by-uuid-or-404 friend-uuid)))
    (db/concept-relation:remove-friend concept friend)
    nil))
