(defsystem "silver-brain"
  :version "1.0.0"
  :author "YUE Daian"
  :license "MIT"
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
               #:mito)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:module "core"
                  :components 
                  ((:file "db")
                   (:file "concept")
                   (:file "concept-relation")
                   (:file "config")))
                 (:module "server"
                  :components
                  ((:file "server-util")
                   (:file "server")
                   (:file "route")))
                 (:file "main"))))
  :description "A Concept Map software that extends your brain storage"
  :in-order-to ((test-op (test-op "silver-brain/tests"))))

(defsystem "silver-brain/tests"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:silver-brain
               #:rove
               #:drakma)
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:module "server"
                  :components
                  ((:file "server"))))))
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
