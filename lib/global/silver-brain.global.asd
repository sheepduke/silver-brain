(defsystem silver-brain.global
  :description "Global variables"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:unlisp #:shasht)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "config")))
