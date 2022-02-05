(defpackage silver-brain.concept-map.store
  (:use #:cl
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:util #:silver-brain.util)
                    (#:store #:silver-brain.store))
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:op
                #:~>>
                #:->)
  (:import-from #:alexandria
                #:if-let)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache))
  (:export #:get-concept-by-uuid
           #:search-concept-by-string
           #:concept-uuid-exists-p
           #:create-database
           #:update-concept
           #:create-concept
           #:delete-concept
           #:create-link
           #:link-uuid-exists-p
           #:delete-link))

(in-package silver-brain.concept-map.store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Database                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> create-databsae (string) t)
(defun create-database (database-name)
  (store:with-database (database-name :auto-create t :auto-migrate t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> get-concept-by-uuid (string) (or null concept))
(defun get-concept-by-uuid (uuid)
  (if-let (concept (store:get 'store:concept uuid))
    (let* ((links (append (store:select 'store:concept-link
                            (sxql:where (:= :source uuid)))
                          (store:select 'store:concept-link
                            (sxql:where (:= :target uuid))))))
      (make-instance 'concept
                     :uuid (store:uuid concept)
                     :name (store:name concept)
                     :content-type (store:content-type concept)
                     :content (store:content concept)
                     :links (mapcar #'concept-link-dao->concept-link links)
                     :create-time (store:object-created-at concept)
                     :update-time (store:object-updated-at concept)))))

(-> concept-uuid-exists-p (string) boolean)
(defun concept-uuid-exists-p (uuid)
  (> (store:count 'store:concept :uuid uuid) 0))

(-> search-concept-by-string (list) concept-summary-list)
(defun search-concept-by-string (searches)
  (let ((conditions (mapcar (op (list :like :name (format nil "%~a%" _)))
                            searches)))
    (~>> (store:select 'store:concept
           (and conditions (sxql:where (cons :and conditions))))
         (mapcar (lambda (concept)
                   (make-instance 'concept-summary
                                  :uuid (store:uuid concept)
                                  :name (store:name concept)))))))

(-> create-concept (&key (:name string)
                         (:content string)
                         (:content-type string)) string)
(defun create-concept (&key name content content-type)
  (let ((uuid (format nil "~a" (uuid:make-v4-uuid))))
    (store:save (make-instance 'store:concept
                               :uuid uuid
                               :name (or name "")
                               :content (or content "")
                               :content-type (or content-type "")))
      uuid))

(-> update-concept (string &key (:name string)
                         (:content string)
                         (:content-type string)) t)
(defun update-concept (uuid &key name content content-type)
  "Update existing concept or insert a new one. The caller must guarantee that "
  (let ((dao (store:get 'store:concept uuid)))
    (and name (setf (store:name dao) name))
    (and content (setf (store:content dao) content))
    (and content-type (setf (store:content-type dao) content-type))
    (store:save dao)
    (cache:update-if-exists uuid name)))

(-> delete-concept (string) t)
(defun delete-concept (uuid)
  "Delete concept by UUID and all the related concepts."
  (store:with-transaction
      (store:delete-by 'store:concept :uuid uuid)
    (store:delete-by 'store:concept-link :source uuid)
    (store:delete-by 'store:concept-link :relation uuid)
    (store:delete-by 'store:concept-link :target uuid))
  (cache:invalidate-if-exists uuid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Link                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> link-uuid-exists-p (string) boolean)
(defun link-uuid-exists-p (uuid)
  (>  (store:count 'store:concept-link :uuid uuid) 0))

(-> create-link (string string string boolean) t)
(defun create-link (source relation target directionalp)
  (let* ((swap-p (and (not directionalp)
                      (string> source target)))
         (fixed-source (if swap-p target source))
         (fixed-target (if swap-p source target)))
    (store:save (make-instance 'store:concept-link
                               :uuid (util:make-uuid)
                               :source fixed-source
                               :relation relation
                               :target fixed-target
                               :directionalp directionalp))))

(defun delete-link (uuid)
  (store:delete-by 'store:concept-link :uuid uuid))

(defun concept-link-dao->concept-link (dao)
  (make-instance 'concept-link
                 :uuid (store:uuid dao)
                 :source (make-concept-summary (store:source dao))
                 :relation (make-concept-summary (store:relation dao))
                 :target (make-concept-summary (store:target dao))))

(defun make-concept-summary (uuid)
  (make-instance 'concept-summary
                 :uuid uuid
                 :name (cache:get-concept-name uuid)))
