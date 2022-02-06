(defpackage silver-brain.store.migration.2.create-new-table
  (:use #:cl)
  (:export #:migration)
  (:import-from #:mito
                #:object-id
                #:object-created-at
                #:object-updated-at))

(in-package silver-brain.store.migration.2.create-new-table)

(mito:deftable concept ()
  ((uuid :col-type :text :accessor uuid)
   (name :col-type :text :accessor name)
   (content :col-type :text :accessor content)
   (content-format :col-type :text :accessor content-format)))

(mito:deftable concept-relation ()
  ((source :col-type :text)
   (target :col-type :text)))

(mito:deftable concept-new ()
  ((name :col-type :text)
   (content-type :col-type :text :initform "")
   (content :col-type :text :initform ""))
  (:keys name)
  (:auto-pk :uuid))

(mito:deftable concept-link ()
  ((source :col-type :text)
   (relation :col-type :text)
   (target :col-type :text)
   (directionalp :col-type :boolean))
  (:keys source relation target)
  (:auto-pk :uuid))

(defun up ()
  ;; Create new table.
  (mito:ensure-table-exists 'concept-new)
  (mito:ensure-table-exists 'concept-link)

  (let ((concepts (mito:select-dao 'concept)))
    ;; Migrate concept table.
    (dolist (concept concepts)
      (mito:insert-dao
       (make-instance 'concept-new
                      :id (string-downcase (uuid concept))
                      :name (name concept)
                      :content-type "text/org"
                      :content (content concept)
                      :created-at (object-created-at concept)
                      :updated-at (object-updated-at concept)))))

  (let ((parent-relation-uuid (make-uuid))
        (relate-relation-uuid (make-uuid)))
    ;; Create default relation concept.
    (mito:insert-dao (make-instance 'concept-new
                                    :id parent-relation-uuid
                                    :name "Contains"))
    (mito:insert-dao (make-instance 'concept-new
                                    :id relate-relation-uuid
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

(defun make-uuid ()
  (string-downcase (format nil "~a" (uuid:make-v4-uuid))))
