(defpackage silver-brain.store.migration.1.create-legacy-table
  (:use #:cl)
  (:export #:migration))

(in-package silver-brain.store.migration.1.create-legacy-table)

(mito:deftable concept ()
  ((uuid :col-type :string)
   (name :col-type :string)
   (content :col-type :string)
   (content-format :col-type :string)))

(mito:deftable concept-relation ()
  ((source :col-type :string)
   (target :col-type :string)))

(defun up ()
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "1.create-legacy-table"
                 :up #'up))
