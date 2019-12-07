(defsystem "silver-brain"
  :version "1.0.0"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (;; Utility.
               #:alexandria #:iterate #:trivia #:cl-arrows #:str #:trivial-types
               #:uuid #:unix-opts
               ;; Logging.
               #:log4cl
               ;; Web related.
               #:chameleon #:find-port #:cl-json
               #:ningle #:clack #:flexi-streams #:mito)
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
                 (:file "config")
                 (:module "core"
                  :components
                  ((:file "concept")))

                 (:module "db"
                  :components
                  ((:file "concept-dao")
                   (:file "concept-relation-dao")
                   (:file "concept")
                   (:file "concept-relation")
                   (:file "concept-map")
                   (:file "core")))

                 (:module "service"
                  :components
                  ((:file "concept-map")))

                 (:module "server"
                  :components
                  ((:file "util")
                   (:file "route")
                   (:file "core")))

                 (:file "main"))))
  :description "A Concept Map software that extends your brain storage"
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
