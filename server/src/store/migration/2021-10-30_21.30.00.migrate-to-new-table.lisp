(defpackage silver-brain.store.migration.2.create-new-table
  (:use #:cl)
  (:export #:migration)
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at))

(in-package silver-brain.store.migration.2.create-new-table)

(mito:deftable concept ()
  ((uuid :col-type :text)
   (name :col-type :text)
   (content :col-type :text)
   (content-format :col-type :text)))

(mito:deftable concept-relation ()
  ((source :col-type :text)
   (target :col-type :text)))

(mito:deftable concept-new ()
  ((uuid :col-type :text
         :primary-key t)
   (name :col-type :text)
   (content-type :col-type :text :initform "")
   (content :col-type :text :initform ""))
  (:keys name))

(mito:deftable concept-link ()
  ((uuid :col-type :text
         :primary-key t)
   (source :col-type :text)
   (relation :col-type :text)
   (target :col-type :text)
   (directionalp :col-type :boolean))
  (:keys source relation target))

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
      (with-slots (source target) relation
        (with-accessors ((created-at object-created-at)
                         (updated-at object-updated-at))
            relation
          (if (link-directional-p source target)
              (ensure-linked source parent-relation-uuid target
                             created-at updated-at)
              (ensure-undirectional-linked source relate-relation-uuid target
                                           created-at updated-at)))))))

(defun ensure-linked (source relation target created-at updated-at
                      &optional (directionalp t))
  (unless (mito:find-dao 'concept-link
                         :source source
                         :relation relation
                         :target target)
    (mito:insert-dao (make-instance 'concept-link
                                    :uuid (uuid:make-v4-uuid)
                                    :source source
                                    :relation relation
                                    :target target
                                    :created-at created-at
                                    :updated-at updated-at
                                    :directionalp directionalp))))

(defun ensure-undirectional-linked (source relation target created-at updated-at)
  (let* ((swap-p (string> source target))
         (fixed-source (if swap-p target source))
         (fixed-target (if swap-p source target)))
    (ensure-linked fixed-source relation fixed-target created-at updated-at nil)))

(defun link-directional-p (source target)
  (null (mito:select-dao 'concept-relation
          (sxql:where (:and (:= :source target)
                            (:= :target source))))))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "2.migrate-to-new-table"
                 :up #'up))
