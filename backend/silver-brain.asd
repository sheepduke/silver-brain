(defsystem "silver-brain"
  :version "1.0.0"
  :author "YUE Daian"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (;; Utility.
               #:alexandria
               #:uuid
               #:iterate
               #:trivia
               #:str
               #:cl-arrows
               #:trivial-types
               #:unix-opts
               #:chameleon
               ;; Logging.
               #:log4cl
               ;; Web related.
               #:find-port
               #:caveman2
               #:clack
               #:flexi-streams
               #:cl-json
               #:mito

               #:silver-brain/main)
  :pathname "src"
  :description "A Concept Map software that extends your brain storage"
  :in-order-to ((test-op (test-op "silver-brain/tests"))))
