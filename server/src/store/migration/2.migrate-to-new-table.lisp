(defpackage silver-brain.store.migration.2.create-new-table
  (:use #:cl)
  (:export #:migration)
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
      (with-slots (uuid name content) concept
        (with-accessors ((created-at object-created-at)
                         (updated-at object-updated-at))
            concept
          (mito:insert-dao (make-instance 'concept-new
                                          :uuid uuid
                                          :name name
                                          :content-type "text/org"
                                          :content content
                                          :created-at created-at
                                          :updated-at updated-at))))))

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
      (with-slots (concept-relation source target) relation
        (with-accessors ((created-at object-created-at)
                         (updated-at object-updated-at))
            relation
          (let* ((bidirectional-linked-p (bidirectional-linked-p source target))
                 (uuid (if bidirectional-linked-p
                           relate-relation-uuid
                           parent-relation-uuid)))
            (insert-concept-link-if-not-exists uuid source target
                                               created-at updated-at)

            (when bidirectional-linked-p
              (insert-concept-link-if-not-exists uuid target source
                                                 created-at updated-at))))))))

(defun bidirectional-linked-p (source target)
  (mito:select-dao 'concept-relation
    (sxql:where (:and (:= :source target)
                      (:= :target source)))))

(defun insert-concept-link-if-not-exists (uuid source target created-at updated-at)
  (unless (mito:find-dao 'concept-link
                         :uuid uuid
                         :source source
                         :target target)
    (mito:insert-dao (make-instance 'concept-link
                                    :uuid uuid
                                    :source source
                                    :target target
                                    :created-at created-at
                                    :updated-at updated-at))))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "2.migrate-to-new-table"
                 :up #'up))
