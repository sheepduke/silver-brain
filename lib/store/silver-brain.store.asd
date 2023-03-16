(defsystem silver-brain.store
  :version "0.1.0"
  :description "Store layer"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:unlisp
               #:uuid #:local-time #:mito #:flexi-streams
               #:silver-brain.global)
  :serial t
  :components ((:module "schema"
                :components
                ((:file "util")
                 (:file "v1")
                 (:file "v2")
                 (:file "schema")))
               (:module "migration"
                :components
                ((:file "util")
                 (:file "v1")
                 (:file "v2")
                 (:file "migration")))
               (:file "store"))
  :in-order-to ((test-op (test-op :silver-brain-tests.store))))
