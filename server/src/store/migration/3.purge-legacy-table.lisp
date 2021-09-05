(defpackage silver-brain.store.migration.3.purge-legacy-table
  (:use #:cl)
  (:export #:migration))

(in-package silver-brain.store.migration.3.purge-legacy-table)

(defun up ()
  ;; Purge legacy tables.
  (mito:execute-sql (sxql:drop-table :concept))
  (mito:execute-sql (sxql:drop-table :concept_relation))

  ;; Rename new table.
  (mito:execute-sql (sxql:alter-table :concept_new (sxql:rename-to :concept))))

(defparameter migration
  (make-instance 'mitogrator:migration
                 :name "3.purge-legacy-table"
                 :up #'up))
