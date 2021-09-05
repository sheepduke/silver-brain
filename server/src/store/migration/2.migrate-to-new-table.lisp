(defpackage silver-brain.store.migration.2.create-new-table
  (:use #:cl)
  (:export #:migration)
  (:import-from #:trivia
                #:match)
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at))

(in-package silver-brain.store.migration.2.create-new-table)

(mito:deftable concept ()
  ((uuid :col-type :string)
   (name :col-type :string)
   (content :col-type :string)
   (content-format :col-type :string)))

(mito:deftable concept-relation ()
  ((source :col-type :string)
   (target :col-type :string)))

(mito:deftable concept-new ()
  ((uuid :col-type :string
         :primary-key t)
   (name :col-type :string)
   (content-type :col-type :string :initform "")
   (content :col-type :string :initform ""))
  (:keys name))

(mito:deftable concept-link ()
  ((uuid :col-type :string)
   (source :col-type :string)
   (target :col-type :string))
  (:keys uuid source target))

(defun up ()
  ;; Create new table.
  (mito:ensure-table-exists 'concept-new)
  (mito:ensure-table-exists 'concept-link)

  (let ((concepts (mito:select-dao 'concept)))
    ;; Migrate concept table.
    (dolist (concept concepts)
      (match concept
        ((concept uuid name content object-created-at object-updated-at)
         (mito:insert-dao (make-instance 'concept-new
                                         :uuid uuid
                                         :name name
                                         :content-type "text/org"
                                         :content content
                                         :created-at object-created-at
                                         :updated-at object-updated-at))))))

  (let ((parent-relation-uuid (uuid:make-v4-uuid))
        (relate-relation-uuid (uuid:make-v4-uuid)))
    ;; Create default relation concept.
    (mito:insert-dao (make-instance 'concept-new
                                    :uuid parent-relation-uuid
                                    :name "Contains"))
    (mito:insert-dao (make-instance 'concept-new
                                    :uuid relate-relation-uuid
                                    :name "Relates"))

    ;; Migrate relation table.
    (dolist (relation (mito:select-dao 'concept-relation))
      (match relation
        ((concept-relation source target object-created-at object-updated-at)
         (let ((uuid (if (is-doubley-linked source target)
                         relate-relation-uuid
                         parent-relation-uuid)))
           (mito:insert-dao (make-instance 'concept-link
                                           :uuid uuid
                                           :source source
                                           :target target
                                           :created-at object-created-at
                                           :updated-at object-updated-at))))))))

(defun is-doubley-linked (source target)
  (mito:select-dao 'concept-relation
    (sxql:where (:and (:= :source target)
                      (:= :target source)))))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "2.migrate-to-new-table"
                 :up #'up))
