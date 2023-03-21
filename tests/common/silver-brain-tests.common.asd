(defsystem silver-brain-tests.common
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:unlisp
               #:lisp-unit2
               #:silver-brain.store)
  :serial t
  :components ((:module "data"
                :components
                ((:file "v1")
                 (:file "v2")))
               (:file "common")))
