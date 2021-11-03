(defpackage silver-brain.concept-map.store
  (:use #:cl
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.store))
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:op
                #:~>>
                #:->)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache))
  (:export #:get-concept-by-uuid
           #:search-concept-by-string
           #:create-database
           #:update-concept
           #:create-concept
           #:used-as-link-p
           #:delete-concept))

(in-package silver-brain.concept-map.store)

(-> create-databsae (string) t)
(defun create-database (database-name)
  (store:with-database (database-name :auto-create t :auto-migrate t)))

(-> get-concept-by-uuid (string) (or null concept))
(defun get-concept-by-uuid (uuid)
  (store:with-current-database
    (match (store:get 'store:concept uuid)
      ((and (store:concept) concept)
       (make-instance 'concept
                      :uuid (store:uuid concept)
                      :name (store:name concept)
                      :content-type (store:content-type concept)
                      :content (store:content concept)
                      :created-at (store:object-created-at concept)
                      :updated-at (store:object-updated-at concept)))
      (_ nil))))

(-> search-concept-by-string (list) concept-summary-list)
(defun search-concept-by-string (searches)
  (store:with-current-database
    (let ((conditions (mapcar (op (list :like :name (format nil "%~a%" _)))
                              searches)))
      (~>> (store:select 'store:concept
             (sxql:where (append '(:or) conditions)))
           (mapcar (lambda (concept)
                     (make-instance 'concept-summary
                                    :uuid (store:uuid concept)
                                    :name (store:name concept))))))))

(-> create-concept (&key (:name string)
                         (:content string)
                         (:content-type string)) string)
(defun create-concept (&key name content content-type)
  (let ((uuid (format nil "~a" (uuid:make-v4-uuid))))
    (store:with-current-database
      (store:save (make-instance 'store:concept
                                 :uuid uuid
                                 :name (or name "")
                                 :content (or content "")
                                 :content-type (or content-type "")))
      uuid)))

(-> update-concept (string &key (:name string)
                         (:content string)
                         (:content-type string)) t)
(defun update-concept (uuid &key name content content-type)
  "Update existing concept or insert a new one. The caller must guarantee that "
  (store:with-current-database
    (let ((dao (store:get 'store:concept uuid)))
      (and name (setf (store:name dao) name))
      (and content (setf (store:content dao) content))
      (and content-type (setf (store:content-type dao) content-type)) 
      (store:save dao))))

(-> used-as-link-p (string) boolean)
(defun used-as-link-p (uuid)
  "Return T is given UUID is used as link concept."
  (store:with-current-database
    (> (store:count 'store:concept-link :uuid uuid) 0)))

(-> delete-concept (string) t)
(defun delete-concept (uuid)
  "Delete concept by UUID and all the related concepts."
  (store:with-current-database
    (store:with-transaction
        (store:delete-by 'store:concept :uuid uuid)
      (store:delete-by 'store:concept-link :source uuid)
      (store:delete-by 'store:concept-link :uuid uuid)
      (store:delete-by 'store:concept-link :target uuid))))
