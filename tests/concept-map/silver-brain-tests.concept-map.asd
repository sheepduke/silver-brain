(defsystem silver-brain-tests.concept-map
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:silver-brain-tests.common
               #:silver-brain.concept-map)
  :serial t
  :components ((:file "concept-map"))
  :perform (test-op (op c)
                    (symbol-call
                     :silver-brain-tests.common
                     :run-tests-in-package
                     :silver-brain-tests.concept-map)))
