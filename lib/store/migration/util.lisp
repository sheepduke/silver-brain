(unlisp.prelude:defpackage #:silver-brain.store.migration.util
  (:use #:unlisp.prelude))

(in-package #:silver-brain.store.migration.util)

(with-auto-export ()
  (defun table-exists? (table-name)
    (mito.db:table-exists-p mito:*connection* table-name)))
