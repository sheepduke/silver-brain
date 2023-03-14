(defsystem silver-brain-tests.store
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:silver-brain-tests.common
               #:silver-brain.store)
  :serial t
  :components ((:module "migration"
                :components
                ((:file "v1")
                 (:file "v2"))))
  :perform (test-op (op c)
                    (symbol-call 'silver-brain-tests.common.util
                                 'run-tagged-tests
                                 :silver-brain.store)))
