(defpackage mitogrator
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export #:run-migrations
           #:migration-name
           #:migration-up
           #:migration-down))
