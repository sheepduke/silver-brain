(unlisp.prelude:defpackage #:silver-brain.store.migration.v1
  (:use #:unlisp.prelude
        #:silver-brain.store.migration.util)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)))

(in-package #:silver-brain.store.migration.v1)

(with-auto-export ()
  (defun run ()
    (when (and (not (table-exists? "meta_info"))
               (not (table-exists? "concept"))
               (not (table-exists? "concept_relation")))
      (mito:ensure-table-exists 'v1:concept)
      (mito:ensure-table-exists 'v1:concept-relation))))
