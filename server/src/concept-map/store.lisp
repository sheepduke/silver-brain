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
           #:save-concept))

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

(-> search-concept-by-string (string) concept-summary-list)
(defun search-concept-by-string (search)
  (store:with-current-database
    (let ((search-string (format nil "%~a%" search)))
      (~>> (store:select 'store:concept
             (sxql:where (:like :name search-string)))
           (mapcar (lambda (concept)
                     (make-instance 'concept-summary
                                    :uuid (store:uuid concept)
                                    :name (store:name concept))))))))

(-> save-concept (concept) t)
(defun save-concept (concept)
  "Update existing concept or insert a new one. The caller must guarantee that "
  (store:with-current-database
    (let ((dao (or (and (uuid concept)
                        (store:get 'store:concept (uuid concept)))
                   (make-instance 'store:concept
                                  :uuid (or (uuid concept)
                                            (uuid:make-v4-uuid))))))
      (setf (store:name dao) (name concept))
      (setf (store:content-type dao) (content-type concept))
      (setf (store:content dao) (content concept))
      (store:save dao))))
