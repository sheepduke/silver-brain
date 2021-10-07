(in-package mitogrator-test)

(def-suite* mitogrator-migrate-unit :in mitogrator)

(test migrate-db-disconnected-signal-error
  (mb:with-dynamic-stubs ((mito.connection:connected-p nil))
    (signals mitogrator:database-not-connected-error
      (mitogrator:migrate '()))))

(test mitgate-empty-db-empty-arg-do-nothing
  (with-mocked-db nil
    (mitogrator:migrate '())
    (is (verify-migration-performed-count 0))))

(test migrate-empty-db-insert-all
  (let* ((history '())
         (migrations (list (make-migration "1" history)
                           (make-migration "2" history))))
    (with-mocked-db nil
      (mitogrator:migrate migrations)
      (is (verify-migration-performed-count 2))
      (is (verify-migration-performed 1 "1"))
      (is (verify-migration-performed 2 "2"))
      (is (equal history '("2" "1"))))))

(test migrate-insert-to-existing-db
  (let* ((history '())
         (migrations (list (make-migration "0" history)
                           (make-migration "1" history)
                           (make-migration "2" history)
                           (make-migration "3" history))))
    (with-mocked-db (make-migration-history "1")
      (mitogrator:migrate migrations)
      (is (verify-migration-performed-count 2))
      (is (verify-migration-performed 1 "2"))
      (is (verify-migration-performed 2 "3"))
      (is (equal history '("3" "2"))))))

(test migrate-fail
  (let* ((history '())
         (migrations (list (make-migration "0" history)
                           (make-migration "1" history)
                           (make-migration "2" history
                                           :up (op
                                                 (push "2" history)
                                                 (error "something wrong"))
                                           :down (op (push "3" history))))))
    (with-mocked-db (make-migration-history "0")
      (handler-case (mitogrator:migrate migrations)
        (error () nil))
      (is (verify-migration-performed-count 1))
      (is (equal history '("3" "2" "1"))))))

(defun verify-migration-performed-count (n)
  (mb:verify-call-times-for 'mitogrator::insert-migration-history n))

(defun verify-migration-performed (n name)
  (let* ((args (mb:nth-mock-args-for n 'mitogrator::insert-migration-history))
         (inserted-name (first args)))
    (string= inserted-name name)))
