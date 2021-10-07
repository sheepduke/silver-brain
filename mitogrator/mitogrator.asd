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
                ((:file "package")
                 (:file "mitogrator"))))
  :in-order-to ((test-op (test-op :mitogrator/test))))

(defsystem mitogrator/test
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:mitogrator
               #:fiveam
               #:mockingbird
               #:serapeum)
  :serial t
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "mitogrator"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :mitogrator :mitogrator-test))))
