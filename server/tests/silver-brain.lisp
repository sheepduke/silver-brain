(defpackage silver-brain-tests
  (:use #:cl)
  (:local-nicknames (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:def-suite*)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:silver-brain
           #:make-random-database-name
           #:with-database
           #:with-random-database-file))

(in-package silver-brain-tests)

(def-suite* silver-brain)

(defun make-random-database-name ()
  (format nil
          "silver-brain-test-~a.sqlite"
          (uuid:make-v4-uuid)))

(defmacro with-random-database-file (&body body)
  (with-gensyms (g-database-name)
    `(let* ((,g-database-name (make-random-database-name))
            (store:*database* ,g-database-name))
       (store:with-database (,g-database-name :auto-create t))
       ,@body
       (uiop:delete-file-if-exists
        (merge-pathnames ,g-database-name
                         (silver-brain.config:data-dir))))))
