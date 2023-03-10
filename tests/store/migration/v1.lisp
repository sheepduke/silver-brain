(unlisp:defpackage #:silver-brain-tests.store.migration.v1
  (:use #:unlisp
        #:lisp-unit2
        #:silver-brain-tests.common.util)
  (:local-nicknames (#:migration #:silver-brain.store.migration)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)))

(in-package #:silver-brain-tests.store.migration.v1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(define-test run (:tags :silver-brain.store
                  :contexts #'temp-db-context)
  (migration:migrate :upto v1:schema-version)

  ;; Check if meta_info is created.
  (string:= (v1:data-version (mito:find-dao 'v1:meta-info))
            v1:schema-version)

  ;; Insert data and check them.
  (list:foreach data.v1:mock-concepts #'mito:insert-dao)
  (list:foreach data.v1:mock-concept-relations #'mito:insert-dao)
  (assert-equal 3 (mito:count-dao 'v1:concept))
  (assert-equal '() (lset:difference data.v1:mock-concepts
                                     (mito:select-dao 'v1:concept)))

  (assert-equal 3 (mito:count-dao 'v1:concept-relation))
  (assert-equal '() (lset:difference data.v1:mock-concept-relations
                                     (mito:select-dao 'v1:concept-relation))))
