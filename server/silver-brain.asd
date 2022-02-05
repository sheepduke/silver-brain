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
               #:flexi-streams
               #:cl-gserver
               #:mitogrator
               #:mito
               #:dbd-sqlite3
               #:ningle
               #:clack
               ;; #:str #:trivial-types
               ;; #:uuid #:unix-opts
               ;; Logging.
               #:log4cl
               #:find-port
               ;; Web related.
               ;; #:chameleon  #:cl-json
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
                    :components ((:file "2021-10-30_21.22.00.create-legacy-table")
                                 (:file "2021-10-30_21.30.00.migrate-to-new-table")
                                 (:file "2021-10-30_21.33.00.purge-legacy-table")
                                 (:file "migrate")))
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
               #:cl-mock
               #:dexador
               #:find-port)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "silver-brain")
                 (:module "store"
                  :components ((:module "migration"
                                :components ((:file "migrate")))))
                 (:module "concept-map"
                  :components ((:file "cache")
                               (:file "store")))
                 (:file "web"))))
  :description "Test system for silver-brain"

  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :silver-brain :silver-brain-tests))))
