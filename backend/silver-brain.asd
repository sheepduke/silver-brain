(defsystem "silver-brain"
  :class :package-inferred-system
  :version "0.1.0"
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:alexandria
               #:uuid
               #:iterate
               #:str
               #:caveman2
               #:clack
               #:flexi-streams
               #:cl-json
               #:trivial-types
               #:unix-opts
               #:envy
               #:mito)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:module "core"
                  :components 
                  ((:file "concept")
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
               #:dexador)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:module "core"
                  :components
                  ((:file "concept")))
                 (:module "server"
                  :components
                  ((:file "server"))))))
  :description "Test system for silver-brain"

  :perform (test-op (op c) (symbol-call :rove :run c)))
