(defpackage silver-brain.store.migration.1.create-legacy-table
  (:use #:cl)
  (:export #:migration))

(in-package silver-brain.store.migration.1.create-legacy-table)

(mito:deftable concept ()
  ((uuid :col-type :text)
   (name :col-type :text)
   (content :col-type :text)
   (content-format :col-type :text)))

(mito:deftable concept-relation ()
  ((source :col-type :text)
   (target :col-type :text)))

(defun up ()
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "1.create-legacy-table"
                 :up #'up))
