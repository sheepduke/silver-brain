(unlisp.prelude:defpackage #:silver-brain-tests.store.migration.v1
  (:use #:unlisp.prelude
        #:lisp-unit2)
  (:local-nicknames (#:migration.v1 #:silver-brain.store.migration.v1)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:data.v1 #:silver-brain-tests.shared.data.v1)))

(in-package #:silver-brain-tests.store.migration.v1)

(unlisp.dev:setup-package-local-nicknames)

(define-test run (:tags 'silver-brain.store
                  :contexts #'temp-db-context)
  (migration.v1:run)
  (list:foreach data.v1:mock-concepts #'mito:insert-dao)
  (list:foreach data.v1:mock-concept-relations #'mito:insert-dao)
  (assert-equal 3 (mito:count-dao 'v1:concept))
  (assert-equal '() (list:set-difference mock-concepts
                                         (mito:select-dao 'v1:concept)
                                         :test #'equal?))

  (assert-equal 3 (mito:count-dao 'v1:concept-relation))
  (assert-equal '() (list:set-difference mock-concept-relations
                                         (mito:select-dao 'v1:concept-relation)
                                         :test #'equal?)))
