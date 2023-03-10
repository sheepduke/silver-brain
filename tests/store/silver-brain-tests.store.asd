(defsystem silver-brain-tests.store
  :license "MIT"
  :author "YUE Daian"
  :depends-on (:silver-brain-tests.common)
  :serial t
  :components ((:module "migration"
                :components
                ((:file "v1")
                 (:file "v2"))))
  :perform (test-op (op c)
                    (lisp-unit2:with-summary ()
                      (lisp-unit2:run-tests :tags '(:silver-brain.store)))))
