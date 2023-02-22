(unlisp.prelude:defpackage #:silver-brain.store.schema.latest
  (:use #:unlisp.prelude
        #:silver-brain.store.schema.v2)
  (:reexport #:silver-brain.store.schema.v2))

(in-package #:silver-brain.store.schema.latest)

(unlisp.dev:setup-package-local-nicknames)

(with-auto-export ()
  (-> fetch-current-version () (nullable string))
  (defun fetch-current-version ()
    "Fetch the current data version from the database. Returns NIL if the meta table does not exist."
    (and (mito.db:table-exists-p mito:*connection* "meta_info")
         (data-version (car (mito:select-dao 'meta-info))))))
