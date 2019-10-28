(defsystem "silver-brain-tests"
  :author "YUE Daian"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (#:silver-brain
               #:rove
               #:drakma
               #:silver-brain-tests/db)
  :pathname "tests"
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
