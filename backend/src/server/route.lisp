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

;; (defroute ("/api/concepts/:uuid/parents" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (db/concept-relation:get-parents-uuid concept)))))

;; (defroute ("/api/concepts/:uuid/parents/:parent-uuid" :method :put)
;;     (&key uuid parent-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (parent (get-concept-by-uuid-or-404 parent-uuid)))
;;     (db/concept-relation:become-child concept parent)
;;     nil))

;; (defroute ("/api/concepts/:uuid/parents/:parent-uuid" :method :delete)
;;     (&key uuid parent-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (parent (get-concept-by-uuid-or-404 parent-uuid)))
;;     (db/concept-relation:remove-child concept parent)
;;     nil))

;; (defroute ("/api/concepts/:uuid/children" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (db/concept-relation:get-children-uuid concept)))))

;; (defroute ("/api/concepts/:uuid/children/:child-uuid" :method :put)
;;     (&key uuid child-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (child (get-concept-by-uuid-or-404 child-uuid)))
;;     (db/concept-relation:become-child child concept)
;;     nil))

;; (defroute ("/api/concepts/:uuid/children/:child-uuid" :method :delete) (&key uuid child-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (child (get-concept-by-uuid-or-404 child-uuid)))
;;     (db/concept-relation:unlink concept child)
;;     nil))

;; (defroute ("/api/concepts/:uuid/friends" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (db/concept-relation:get-friends-uuid concept)))))

;; (defroute ("/api/concepts/:uuid/friends/:friend-uuid" :method :put) (&key uuid friend-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (friend (get-concept-by-uuid-or-404 friend-uuid)))
;;     (db/concept-relation:become-friend concept friend)
;;     nil))

;; (defroute ("/api/concepts/:uuid/friends/:friend-uuid" :method :delete) (&key uuid friend-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (friend (get-concept-by-uuid-or-404 friend-uuid)))
;;     (db/concept-relation:remove-friend concept friend)
;;     nil))
