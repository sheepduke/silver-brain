(unlisp:defpackage #:silver-brain.store.migration.v1
  (:use #:unlisp.prelude)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)))

(in-package #:silver-brain.store.migration.v1)

(with-auto-export ()
  (defun run ()
    (dbi:with-transaction mito:*connection*
      (mito:ensure-table-exists 'v1:meta-info)
      (mito:ensure-table-exists 'v1:concept)
      (mito:ensure-table-exists 'v1:concept-relation)

      (mito:create-dao 'v1:meta-info :data-version v1:schema-version))))
