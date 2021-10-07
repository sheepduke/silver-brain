(defsystem mitogrator
  :version "0.1.0"
  :description "Database migration tool built upon Mito"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria
               #:serapeum
               #:mito)
  :serial t
  :components ((:module "src"
                :components
                ((:file "mitogrator"))))
  :in-order-to ((test-op (test-op :mitogrator-tests))))

(defsystem mitogrator-tests
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:mitogrator
               #:fiveam
               #:cl-mock
               #:serapeum)
  :serial t
  :components ((:module "test"
                :components
                ((:file "mitogrator"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :mitogrator :mitogrator-tests))))
