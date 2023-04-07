(unlisp:defpackage #:silver-brain.store.migration
  (:use #:unlisp
        #:silver-brain.store.migration.util)
  (:local-nicknames (#:schema #:silver-brain.store.schema)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:v2 #:silver-brain.store.schema.v2)
                    (#:migration.v1 #:silver-brain.store.migration.v1)
                    (#:migration.v2 #:silver-brain.store.migration.v2)))

(in-package #:silver-brain.store.migration)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(def migration-map
  (list (cons v2:schema-version #'migration.v2:run)
        (cons v1:schema-version #'migration.v1:run)))

(with-auto-export ()
  (defun migrate (&key (upto schema:latest-schema-version))
    ;; Initialize the database if it is empty.
    (unless (table-exists? "meta_info")
      (mito:ensure-table-exists 'schema:meta-info)
      (mito:create-dao 'schema:meta-info :data-version schema:new-schema-version))

    ;; Migrate to data version by calling corresponding migration task.
    ;; It results in a chained invocation.
    (mv-ematch (alist:elt migration-map upto)
      ((_ nil) (error "Unrecognized target version: ~A" upto))
      ((runner t) (funcall runner)))))

(reexport-from #:silver-brain.store.migration.util
  #:table-exists? #:fetch-data-version)