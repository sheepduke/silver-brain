(unlisp:defpackage #:silver-brain-tests.shared.context
  (:use #:unlisp)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)
                    (#:data-v1 #:silver-brain-tests.shared.data.v1)))

(in-package #:silver-brain-tests.shared.context)

(unlisp.dev:setup-package-local-nicknames)

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
        (os:ensure-file-deleted filepath))))

  (defun v1-test-data-context (fun)
    (mito:ensure-table-exists 'v1:concept)
    (mito:ensure-table-exists 'v1:concept-relation)
    (list:foreach data-v1:mock-concepts #'mito:insert-dao)
    (list:foreach data-v1:mock-concept-relations #'mito:insert-dao)
    (funcall fun)))
