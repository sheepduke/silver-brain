(defsystem "silver-brain"
  :version "0.1.0"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (:uuid :iterate)
  :components ((:module "src"
                :components
                ((:file "concept")
                 (:file "main"))))
  :description "A Concept Map software that extends your brain storage"
  :in-order-to ((test-op (test-op "silver-brain/tests"))))

(defsystem "silver-brain/tests"
  :author "YUE Daian"
  :license "MIT"
  :depends-on ("silver-brain"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
