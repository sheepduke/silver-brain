(defsystem silver-brain.global
  :description "Global variables"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:unlisp)
  :serial t
  :components ((:file "package")
               (:file "runtime-settings")))
