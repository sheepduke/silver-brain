(defpackage silver-brain.store.migration
  (:nicknames store.migration)
  (:use #:cl)
  (:export
   #:run-migrations))

(in-package silver-brain.store.migration)

(defun run-migrations ()
  (let ((migrations (list silver-brain.store.migration.1.create-legacy-table:migration
                          silver-brain.store.migration.2.create-new-table:migration
                          silver-brain.store.migration.3.purge-legacy-table:migration)))
    (mitogrator:run migrations)))
