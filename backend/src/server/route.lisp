(defpackage silver-brain/server/route
  (:use #:cl
        #:silver-brain/server/util
        #:silver-brain/db/concept
        #:silver-brain/db/concept-relation)
  (:import-from #:caveman2
                #:defroute)
  (:import-from #:ningle
                #:route)
  (:export #:*app*))

(in-package silver-brain/server/route)

(defun get-concept-by-uuid-or-404 (uuid)
  (let ((concept (get-concept-by-uuid uuid)))
    (or concept
        (throw-code 404))
    concept))

(defvar *app*
  (make-instance 'caveman2:<app>))

(caveman2:clear-routing-rules *app*)

;; (defroute ("/" :method :get) ()
;;   #P"index.html")

;; (defroute ("/concepts" :method :get) (&key _parsed)
;;   (let ((search (assoc-value _parsed "search" :test #'string=)))
;;     (render-json-array
;;      (-<>> (if search (find-concepts-by-name search) (get-all-concepts))
;;        (mapcar #'concept-summary <>)))))

;; (defroute ("/concepts" :method :post) ()
;;   (match (decode-request-json-alist '(:name :content :content-format))
;;     (nil (throw-code 400))
;;     ((list name content content-format)
;;      (let ((concept (add-concept name content content-format)))
;;        (set-response-location-header
;;         (format nil "/concepts/~a" (concept-uuid concept)))
;;        (set-response-status 201)
;;        nil))))

;; (defroute ("/concepts/:uuid" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json
;;      `((:uuid . ,(concept-uuid concept))
;;        (:name . ,(concept-name concept))
;;        (:content . ,(concept-content concept))
;;        (:content-format . ,(concept-content-format concept))))))

;; (defroute ("/concepts/:uuid" :method :put) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (match (decode-request-json-alist '(:name :content :content-format))
;;       (nil (throw-code 400))
;;       ((list name content content-format)
;;        (setf (concept-name concept) name)
;;        (setf (concept-content concept) content)
;;        (setf (concept-content-format concept) content-format)
;;        (save-concept concept)))))

;; (defroute ("/concepts/:uuid" :method :delete) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (delete-concept concept)
;;     nil))

;; (defroute ("/concepts/:uuid/parents" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (get-concept-parents concept)))))

;; (defroute ("/concepts/:uuid/parents/:parent-uuid" :method :put)
;;     (&key uuid parent-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (parent (get-concept-by-uuid-or-404 parent-uuid)))
;;     (become-child parent concept)
;;     nil))

;; (defroute ("/concepts/:uuid/parents/:parent-uuid" :method :delete)
;;     (&key uuid parent-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (parent (get-concept-by-uuid-or-404 parent-uuid)))
;;     (remove-child parent concept)
;;     nil))

;; (defroute ("/concepts/:uuid/children" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (get-concept-children concept)))))

;; (defroute ("/concepts/:uuid/children/:child-uuid" :method :put)
;;     (&key uuid child-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (child (get-concept-by-uuid-or-404 child-uuid)))
;;     (become-child concept child)
;;     nil))

;; (defroute ("/concepts/:uuid/children/:child-uuid" :method :delete) (&key uuid child-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (child (get-concept-by-uuid-or-404 child-uuid)))
;;     (remove-child concept child)
;;     nil))

;; (defroute ("/concepts/:uuid/children/:child-uuid" :method :delete) (&key uuid child-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (child (get-concept-by-uuid-or-404 child-uuid)))
;;     (remove-relations concept child)
;;     nil))

;; (defroute ("/concepts/:uuid/friends" :method :get) (&key uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid)))
;;     (render-json-array
;;      (mapcar #'concept-summary
;;              (get-concept-friends concept)))))

;; (defroute ("/concepts/:uuid/friends/:friend-uuid" :method :put) (&key uuid friend-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (friend (get-concept-by-uuid-or-404 friend-uuid)))
;;     (become-friend concept friend)
;;     nil))

;; (defroute ("/concepts/:uuid/friends/:friend-uuid" :method :delete) (&key uuid friend-uuid)
;;   (let ((concept (get-concept-by-uuid-or-404 uuid))
;;         (friend (get-concept-by-uuid-or-404 friend-uuid)))
;;     (remove-friend concept friend)
;;     nil))
