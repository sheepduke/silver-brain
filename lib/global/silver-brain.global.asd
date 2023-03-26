(defsystem silver-brain.global
  :description "Global variables"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:unlisp #:shasht #:chameleon)
  :serial t
  :components ((:file "package")
               (:file "config")))
