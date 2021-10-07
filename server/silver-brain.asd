(defsystem silver-brain
  :version "1.0.0"
  :description "A Concept Map software that extends your brain storage"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (;; Utility
               #:alexandria
               #:serapeum
               #:trivia
               ;; Application
               #:chameleon
               #:uuid
               #:jsown
               #:cl-gserver
               #:mitogrator
               #:mito
               #:dbd-sqlite3
               #:ningle
               #:clack
               ;; #:str #:trivial-types
               ;; #:uuid #:unix-opts
               ;; Logging.
               ;; #:log4cl
               ;; Web related.
               ;; #:chameleon #:find-port #:cl-json
               #:ningle #:clack ;; #:flexi-streams
               )
  :serial t
  :components ((:module "src"
                :serial t
                :components
                (;; Global files.
                 (:file "config")
                 (:file "util")
                 ;; Store.
                 (:module "store"
                  :components
                  ((:module "migration"
                    :components ((:file "1.create-legacy-table")
                                 (:file "2.migrate-to-new-table")
                                 (:file "3.purge-legacy-table")
                                 (:file "migrate")))
                   (:file "connection")
                   (:file "store")))
                 (:module "concept-map"
                  :components ((:file "model")
                               (:file "cache")
                               (:file "store")
                               (:file "concept-map")))
                 (:file "web")
                 (:file "silver-brain"))))
  :in-order-to ((test-op (test-op :silver-brain-tests))))

(defsystem silver-brain-tests
  :author "YUE Daian"
  :license "MIT"
  :depends-on (#:silver-brain
               #:fiveam
               #:cl-mock)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "suite")
                 (:module "concept-map"
                  :components ((:file "cache")))
                 (:module "store"
                  :components ((:file "connection"))))))
  :description "Test system for silver-brain"

  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :silver-brain :silver-brain-tests))))
