(unlisp:defpackage #:silver-brain-tests.common.util
  (:use #:unlisp)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)
                    (#:migration.v1 #:silver-brain.store.migration.v1)))

(in-package #:silver-brain-tests.common.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (defun temp-db-context (fun)
    (let* ((directory (path:join (path:temporary-directory) "silver-brain-tests/"))
           (filepath (format nil "~A/~A.sqlite" directory (uuid:make-v4-uuid))))

      (os:ensure-directories-exist directory)

      (unwind-protect
           (let ((mito:*connection* nil)
                 (mito:*trace-sql-hooks* nil))
             (dbi:with-connection (mito:*connection* :sqlite3
                                                     :database-name filepath)
               (funcall fun)))
        (os:ensure-file-deleted filepath)))))
