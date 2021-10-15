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
           #:search-concept-by-string))

(in-package silver-brain.concept-map.store)

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
