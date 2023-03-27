(unlisp:defpackage #:silver-brain.store
  (:use #:unlisp
        #:silver-brain.store.migration
        #:silver-brain.store.schema)
  (:local-nicknames (#:global #:silver-brain.global))
  (:reexport #:silver-brain.store.migration
             #:silver-brain.store.schema))

(in-package #:silver-brain.store)

 (eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (defun ensure-data-hierarchy-exists ()
    (os:ensure-directories-exist (global:store/attachments-path)))

  (defun call-with-database (database-name function)
    (let ((database-path (global:store/database-path database-name)))
      (let ((mito:*connection* (dbi:connect-cached :sqlite3
                                                   :database-name database-path)))
        (unwind-protect (funcall function)
          (dbi:disconnect mito:*connection*)))))

  (defmacro with-database (database-name &body body)
    `(call-with-database ,database-name
                         (fun () ,@body))))
