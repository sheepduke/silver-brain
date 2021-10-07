(defpackage silver-brain-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite*)
  (:export #:silver-brain
           #:make-random-database-name))

(in-package silver-brain-tests)

(def-suite* silver-brain)

(defun make-random-database-name ()
  (format nil
          "~asilver-brain-test-~a.sqlite"
          (uiop:temporary-directory)
          (uuid:make-v4-uuid)))

(defmacro with-database ((database-name) &body body)
  `(dbi:with-connection (mito:*connection* :sqlite3 :database-name ,database-name)
     ,@body))
