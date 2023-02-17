(defsystem silver-brain.store
  :version "0.1.0"
  :description "Store layer"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:unlisp
               #:local-time)
  :serial t
  :components ((:module "schema"
                :components
                ((:file "v1"))))
  :in-order-to ((test-op (test-op :silver-brain.store.tests))))

(defsystem silver-brain.store.tests
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (:silver-brain.store
               :fiveam)
  :serial t
  :components ((:module "test"
                :components
                ((:file "silver-brain.store"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :silver-brain.store
                                               :silver-brain.store.tests))))
