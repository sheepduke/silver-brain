(unlisp.prelude:defpackage #:silver-brain.store.migration.v1
  (:use #:unlisp.prelude
        #:silver-brain.store.schema.v1))

(in-package #:silver-brain.store.migration.v1)

(with-auto-export ()
  (defun run ()
    ;; TODO: add checks for meta_info
    (mito:ensure-table-exists 'concept)
    (mito:ensure-table-exists 'concept-relation)))
