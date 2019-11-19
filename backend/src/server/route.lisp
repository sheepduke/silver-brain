(in-package silver-brain.server)

(defroute *app* GET "/" ()
  #P"index.html")

(defroute *app* GET "/api" ()
  (set-response-status 200)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute *app* GET "/api/concepts" (search)
  (render-json-array
   (-<>> (if search
             (service:find-concept-by-name search)
             (service:get-all-concepts))
         (mapcar #'concept-summary <>))))

(defroute *app* POST "/api/concepts" ()
  (match (decode-request-json-alist '(:name :content :content-format))
    ((list name content content-format)
     (let ((concept (service:create-concept name content content-format)))
       (set-response-location
        (format nil "/concepts/~a" (concept-uuid concept)))
       (set-response-status 201)
       nil))))

(defroute *app* GET "/api/concepts/:uuid" (uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (render-json
     `((:uuid . ,(concept-uuid concept))
       (:name . ,(concept-name concept))
       (:content . ,(concept-content concept))
       (:content-format . ,(concept-content-format concept))))))

(defroute *app* PUT "/api/concepts/:uuid" (uuid)
  (get-concept-by-uuid-or-404 uuid)
  (match (decode-request-json-alist '(:name :content :content-format))
    ((list name content content-format)
     (service:update-concept uuid
                             :name name
                             :content content
                             :content-format content-format)))
  nil)

(defroute *app* DELETE "/api/concepts/:uuid" (uuid)
  (get-concept-by-uuid-or-404 uuid)
  (service:delete-concept uuid)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Concept Relation                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute *app* GET "/api/concepts/:uuid/parents" (uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (->> (concept-parents concept)
         (mapcar (lambda (uuid) (service:get-concept-by-uuid uuid)))
         (mapcar #'concept-summary)
         (render-json-array))))

(defroute *app* PUT "/api/concepts/:uuid/parents/:parent-uuid" (uuid parent-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (parent (get-concept-by-uuid-or-404 parent-uuid)))
    (service:make-child parent concept)
  nil))

(defroute *app* DELETE "/api/concepts/:uuid/parents/:parent-uuid" (uuid parent-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (parent (get-concept-by-uuid-or-404 parent-uuid)))
    (when (concept-childp parent concept)
      (service:remove-relation concept parent))
    nil))

(defroute *app* GET "/api/concepts/:uuid/children" (uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (->> (concept-children concept)
         (mapcar (lambda (uuid) (service:get-concept-by-uuid uuid)))
         (mapcar #'concept-summary)
         (render-json-array))))

(defroute *app* PUT "/api/concepts/:uuid/children/:child-uuid" (uuid child-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (child (get-concept-by-uuid-or-404 child-uuid)))
    (service:make-child child concept)
  nil))

(defroute *app* DELETE "/api/concepts/:uuid/children/:child-uuid" (uuid child-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (child (get-concept-by-uuid-or-404 child-uuid)))
    (when (concept-childp concept child)
      (service:remove-relation concept child))
    nil))

(defroute *app* GET "/api/concepts/:uuid/friends" (uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid)))
    (->> (concept-friends concept)
         (mapcar (lambda (uuid) (service:get-concept-by-uuid uuid)))
         (mapcar #'concept-summary)
         (render-json-array))))

(defroute *app* PUT "/api/concepts/:uuid/friends/:friend-uuid" (uuid friend-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (friend (get-concept-by-uuid-or-404 friend-uuid)))
    (service:make-friend friend concept)
  nil))

(defroute *app* DELETE "/api/concepts/:uuid/friends/:friend-uuid" (uuid friend-uuid)
  (let ((concept (get-concept-by-uuid-or-404 uuid))
        (friend (get-concept-by-uuid-or-404 friend-uuid)))
    (when (concept-friendp concept friend)
      (service:remove-relation concept friend))
    nil))
