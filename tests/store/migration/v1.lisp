(unlisp.prelude:defpackage #:silver-brain-tests.store.migration.v1
  (:use #:unlisp.prelude
        #:lisp-unit2)
  (:local-nicknames (#:migration.v1 #:silver-brain.store.migration.v1)
                    (#:v1 #:silver-brain.store.schema.v1)))

(in-package #:silver-brain-tests.store.migration.v1)

(unlisp.dev:setup-package-local-nicknames)

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

(def mock-concept-01 (make-instance 'v1:concept
                              :uuid "0x01"
                              :name "First one"
                              :content-format "text/org"
                              :content "Org content"))

(def mock-concept-02 (make-instance 'v1:concept
                              :uuid "0x02"
                              :name "Second one"
                              :content-format "text/md"
                              :content "Markdown content"))

(def mock-concept-03 (make-instance 'v1:concept
                              :uuid "0x03"
                              :name "Third one"))

(def mock-concepts (list mock-concept-01 mock-concept-02 mock-concept-03))

(def mock-relation-01->02 (make-instance 'v1:concept-relation
                                    :source "0x01"
                                    :target "0x02"))

(def mock-relation-01->03 (make-instance 'v1:concept-relation
                                    :source "0x01"
                                    :target "0x03"))

(def mock-relation-03->01 (make-instance 'v1:concept-relation
                                    :source "0x03"
                                    :target "0x01"))

(def mock-concept-relations (list mock-relation-01->02
                                  mock-relation-01->03
                                  mock-relation-03->01))

(defun v1-test-data-context (fun)
  (migration.v1:run)
  (list:foreach mock-concepts #'mito:insert-dao)
  (list:foreach mock-concept-relations #'mito:insert-dao)
  (funcall fun))

(define-test run (:tags 'silver-brain.store
                  :contexts (list #'temp-db-context
                                  #'v1-test-data-context))
  (assert-equal 3 (mito:count-dao 'v1:concept))
  (assert-equal '() (list:set-difference mock-concepts
                                         (mito:select-dao 'v1:concept)
                                         :test #'equal?))

  (assert-equal 3 (mito:count-dao 'v1:concept-relation))
  (assert-equal '() (list:set-difference mock-concept-relations
                                         (mito:select-dao 'v1:concept-relation)
                                         :test #'equal?)))
