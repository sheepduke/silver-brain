(defsystem "silver-brain"
  :version "0.1.0"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:alexandria
               #:uuid
               #:iterate
               #:ningle
               #:clack)
  :components ((:module "src"
                :components
                ((:file "concept")
                 (:file "concept-map")
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
                ((:file "concept")
                 (:file "concept-map")
                 (:file "main"))))
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
