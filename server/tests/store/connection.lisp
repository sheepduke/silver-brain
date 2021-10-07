(defpackage silver-brain-tests.store.connection
  (:use #:cl)
  (:local-nicknames (#:connection #:silver-brain.store.connection)
                    (#:migration #:silver-brain.store.migration))
  (:import-from #:fiveam
                #:is
                #:def-suite*
                #:def-test)
  (:import-from #:cl-mock
                #:invocations
                #:answer
                #:with-mocks))

(in-package silver-brain-tests.store.connection)

(def-suite* silver-brain.store.connection :in silver-brain-tests:silver-brain)

(def-test update-connections ()
  (with-mocks ()
    (let ((connections (make-hash-table :test #'equal))
          (existing-database "1")
          (non-existing-database "2"))
      ;; Setup.
      (answer (dbi:connect :sqlite3 :database-name "1") :connected-1)
      (answer (uiop:file-exists-p "1") t)
      (answer (dbi:connect :sqlite3 :database-name "2") :connected-2)
      (answer (uiop:file-exists-p "2") nil)
      (answer (migration:run-migrations))

      ;; Verify first invocation.
      (connection::update-connections existing-database connections)
      (is (= 1 (hash-table-count connections)))
      (is (eql :connected-1 (gethash existing-database connections)))

      ;; Verify second invocation.
      (connection::update-connections non-existing-database connections)
      (is (= 1 (hash-table-count connections)))
      (is (eql nil (gethash non-existing-database connections)))

      ;; Verify final state.
      (connection::update-connections existing-database connections)
      (is (= 1 (hash-table-count connections)))
      (is (eql :connected-1 (gethash existing-database connections)))
      (is (eql nil (gethash non-existing-database connections)))

      ;; Verify same connection is not made twice.
      (is (equal `((dbi:connect :sqlite3 :database-name ,existing-database))
                 (invocations 'dbi:connect))))))

(def-test disconnect-all ()
  (with-mocks ()
    (answer dbi:disconnect)
    (let ((connections (make-hash-table :test #'equal)))
      (setf (gethash "1" connections) :connected-1)
      (setf (gethash "2" connections) :connected-2)
      (setf connections (connection::disconnect-all connections))
      (is (null (set-difference '((dbi:disconnect :connected-1)
                                  (dbi:disconnect :connected-2))
                                (invocations 'dbi:disconnect)
                                :test #'equal)))
      (is (= 0 (hash-table-count connections))))))

(def-test get ()
  (flet ((make-random-database-name ()
           (format nil
                   "~asilver-brain-test-~a.sqlite"
                   (uiop:temporary-directory)
                   (uuid:make-v4-uuid))))
    (let ((existing-database (make-random-database-name))
          (non-existing-database (make-random-database-name)))
      (connection:start)
      (dbi:with-connection (connection :sqlite3 :database-name existing-database))

      (is (string= existing-database
                   (dbi:connection-database-name
                    (connection:get existing-database))))
      
      (is (null (connection:get non-existing-database)))

      (is (string= existing-database
                   (dbi:connection-database-name
                    (connection:get existing-database))))

      ;; Cleanup
      (connection:stop)
      (uiop:delete-file-if-exists existing-database)
      (uiop:delete-file-if-exists non-existing-database))))

(5am:run! 'silver-brain.store.connection)
