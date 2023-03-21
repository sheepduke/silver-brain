(defsystem silver-brain-tests.store
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:silver-brain-tests.common
               #:silver-brain.store)
  :serial t
  :components ((:file "package")
               (:module "migration"
                :components
                ((:file "v1")
                 (:file "v2"))))
  :perform (test-op (op c)
                    (symbol-call
                     :silver-brain-tests.common
                     :run-tests-in-package
                     :silver-brain-tests.store)))
