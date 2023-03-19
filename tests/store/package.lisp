(unlisp:defpackage #:silver-brain-tests.store
  (:use #:unlisp
        #:lisp-unit2
        #:silver-brain-tests.common.util)
  (:local-nicknames (#:global #:silver-brain.global)
                    (#:migration #:silver-brain.store.migration)
                    (#:migration #:silver-brain.store.migration)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:v2 #:silver-brain.store.schema.v2)))

(in-package #:silver-brain-tests.store)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))
