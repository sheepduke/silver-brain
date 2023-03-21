(defsystem silver-brain.concept-map
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:unlisp #:uuid #:local-time #:mito #:jsown
               #:silver-brain.store)
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "concept-map"))
  :in-order-to ((test-op (test-op :silver-brain-tests.concept-map))))
