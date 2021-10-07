(defpackage mitogrator-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:signals
                #:test
                #:def-suite*
                #:is)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:serapeum
                #:op)
  (:import-from #:cl-mock
                #:invocations
                #:answer
                #:with-mocks)
  (:export #:mitogrator))

(in-package mitogrator-tests)

(def-suite* mitogrator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Utilities                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-migration (name history &key up down)
  (with-gensyms (g-name g-up g-down)
    `(let ((,g-name ,name)
           (,g-up ,up)
           (,g-down ,down))
       (make-instance 'mitogrator:migration
                      :name ,g-name
                      :up (if ,g-up ,g-up (op (push ,g-name ,history)))
                      :down (if ,g-down ,g-down)))))

(defun make-migration-history (name)
  (make-instance 'mitogrator:migration-history :name name))

(defun setup-default-mocks ()
  (answer mito.connection:connected-p t)
  (answer mito:ensure-table-exists)
  (answer mitogrator::select-latest-migration-history)
  (answer dbi:begin-transaction)
  (answer dbi:commit)
  (answer dbi:rollback)
  (answer mito:insert-dao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Tests                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test run-db-disconnected-signal-error
  (with-mocks ()
    (answer (mito.connection:connected-p) nil)
    (signals (mitogrator:database-not-connected-error)
      (mitogrator:run '()))))

(test mitgate-empty-db-empty-arg-do-nothing
  (with-mocks ()
    (setup-default-mocks)
    (mitogrator:run '())
    (is (null (invocations 'mito:insert-dao)))))

(test run-empty-db-insert-all
  (let* ((history '())
         (migrations (list (make-migration "1" history)
                           (make-migration "2" history)))
         (migration-history '())
         (expected-history '("2" "1")))
    (with-mocks ()
      (answer (mito:insert-dao history)
        (push (mitogrator:name history) migration-history))
      (setup-default-mocks)
      
      (mitogrator:run migrations)

      (let ((invocations (invocations 'mito:insert-dao)))
        ;; Verify migration functions are called.
        (is (equal expected-history history))
        ;; Verify migration history is recorded.
        (is (equal expected-history migration-history))
        (is (= 2 (length invocations)))))))

(test run-insert-to-existing-db
  (let* ((history '())
         (migration-history '())
         (migrations (list (make-migration "0" history)
                           (make-migration "1" history)
                           (make-migration "2" history)
                           (make-migration "3" history)))
         (expected-history '("3" "2")))
    (with-mocks ()
      (answer (mitogrator::select-latest-migration-history)
        (make-migration-history "1"))
      (answer (mito:insert-dao history)
        (push (mitogrator:name history) migration-history))
      (setup-default-mocks)
      
      (mitogrator:run migrations)

      (is (equal expected-history migration-history))
      (is (equal expected-history history)))))

(test run-fail
  (let* ((history '())
         (migration-history '())
         (migrations (list (make-migration "0" history)
                           (make-migration "1" history)
                           (make-migration "2" history
                                           :up (op
                                                 (push "2" history)
                                                 (error "something wrong"))))))
    (with-mocks ()
      (answer (mitogrator::select-latest-migration-history)
        (make-migration-history "0"))
      (answer (mito:insert-dao dao)
        (push dao migration-history))
      (setup-default-mocks)

      (signals error (mitogrator:run migrations))

      (let ((invocations (invocations 'mito:insert-dao)))
        (is (= 1 (length invocations)))
        (is (= 1 (length migration-history)))
        (is (string= "1" (mitogrator:name (first migration-history))))))))
