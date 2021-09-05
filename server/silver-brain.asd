(defsystem silver-brain
  :version "1.0.0"
  :description "A Concept Map software that extends your brain storage"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (;; Utility
               #:alexandria
               #:serapeum
               #:uuid

               ;; Database
               #:mitogrator
               ;; #:trivia #:str #:trivial-types
               ;; #:uuid #:unix-opts
               ;; Logging.
               ;; #:log4cl
               ;; Web related.
               ;; #:chameleon #:find-port #:cl-json
               ;; #:ningle #:clack #:flexi-streams #:mito
               )
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
                 (:file "silver-brain"))))
  :in-order-to ((test-op (test-op "silver-brain/tests"))))

(defsystem "silver-brain/tests"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:rove
               #:drakma
               #:silver-brain)
  :components ((:module "tests"
                :components
                ((:file "packages")

                 (:module "core"
                  :components
                  ((:file "concept")))

                 (:module "server"
                  :components
                  ((:file "server"))))))
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
