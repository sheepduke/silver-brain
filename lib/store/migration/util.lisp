(unlisp:defpackage #:silver-brain.store.migration.util
  (:use #:unlisp)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)
                    (#:schema #:silver-brain.store.schema)))

(in-package #:silver-brain.store.migration.util)

(unlisp.dev:setup-package-local-nicknames)

(with-auto-export ()
  (defun table-exists? (table-name)
    (mito.db:table-exists-p mito:*connection* table-name))

  (defun fetch-data-version ()
    (schema:data-version (mito:find-dao 'schema:meta-info))))
