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
  (if-let (dao (store:get 'store:concept uuid))
    (let* ((concept (concept-dao->concept dao))
           (links (store:select 'store:concept-link
                    (sxql:where (:or (:= :source uuid)
                                     (:= :target uuid))))))
      (setf (links concept)
            (mapcar #'concept-link-dao->concept-link links))
      concept)))

(-> concept-uuid-exists-p (string) boolean)
(defun concept-uuid-exists-p (uuid)
  (> (store:count 'store:concept :id uuid) 0))

(-> search-concept-by-string (list) concept-summary-list)
(defun search-concept-by-string (searches)
  (let ((conditions (mapcar (op (list :like :name (format nil "%~a%" _)))
                            searches)))
    (~>> (store:select 'store:concept
           (and conditions (sxql:where (cons :and conditions))))
         (mapcar (lambda (concept)
                   (make-instance 'concept-summary
                                  :uuid (store:object-id concept)
                                  :name (store:name concept)))))))

(-> create-concept (&key (:name string)
                         (:content string)
                         (:content-type string))
  concept)
(defun create-concept (&key name content content-type)
  (let ((concept (make-instance 'store:concept
                                :name (or name "")
                                :content (or content "")
                                :content-type (or content-type ""))))
    (store:save concept)
    (concept-dao->concept concept)))

(-> update-concept (string &key (:name string)
                         (:content string)
                         (:content-type string)) t)
(defun update-concept (uuid &key name content content-type)
  "Update existing concept or insert a new one. The caller must guarantee that "
  (let ((dao (store:get 'store:concept uuid)))
    (and name (setf (store:name dao) name))
    (and content (setf (store:content dao) content))
    (and content-type (setf (store:content-type dao) content-type))
    (store:save dao)))

(-> delete-concept (string) t)
(defun delete-concept (uuid)
  "Delete concept by UUID and all the related concepts."
  (store:with-transaction
      (store:delete-by 'store:concept :id uuid)
    (store:delete-by 'store:concept-link :source uuid)
    (store:delete-by 'store:concept-link :relation uuid)
    (store:delete-by 'store:concept-link :target uuid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Link                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> link-uuid-exists-p (string) boolean)
(defun link-uuid-exists-p (uuid)
  (>  (store:count 'store:concept-link :id uuid) 0))

(-> create-link (string string string boolean) concept-link)
(defun create-link (source relation target directionalp)
  (let* ((swap-p (and (not directionalp)
                      (string> source target)))
         (fixed-source (if swap-p target source))
         (fixed-target (if swap-p source target))
         (link (make-instance 'store:concept-link
                              :source fixed-source
                              :relation relation
                              :target fixed-target
                              :directionalp directionalp)))
    (store:save link)
    (concept-link-dao->concept-link link)))

(defun delete-link (uuid)
  (store:delete-by 'store:concept-link :id uuid))

(defun concept-link-dao->concept-link (dao)
  (make-instance 'concept-link
                 :uuid (store:object-id dao)
                 :source (make-concept-summary (store:source dao))
                 :relation (make-concept-summary (store:relation dao))
                 :target (make-concept-summary (store:target dao))
                 :directionalp (store:directionalp dao)))

(defun concept-dao->concept (dao)
  (make-instance 'concept
                 :uuid (store:object-id dao)
                 :name (store:name dao)
                 :content-type (store:content-type dao)
                 :content (store:content dao)
                 :create-time (store:object-created-at dao)
                 :update-time (store:object-updated-at dao)))

(defun make-concept-summary (uuid)
  (make-instance 'concept-summary
                 :uuid uuid
                 :name (get-concept-name uuid)))

(defun get-concept-name (uuid)
  (store:name (mito:find-dao 'concept :uuid uuid)))
