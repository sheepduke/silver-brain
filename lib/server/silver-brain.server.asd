(defsystem silver-brain.server
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:unlisp #:jingle #:lack-middleware-accesslog
               #:silver-brain.concept-map
               #:silver-brain.global)
  :serial t
  :components ((:file "package")
               (:file "server-dev"))
  :in-order-to ((test-op (test-op :silver-brain-tests.server))))

(defsystem silver-brain-tests.server
  :version "0.1.0"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:silver-brain.server
               #:lisp-unit2)
  :serial t
  :components ()
  :perform (test-op (op c)
                    (symbol-call :silver-brain-tests.server
                                 :run-all-tests)))
