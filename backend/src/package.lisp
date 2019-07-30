(defpackage silver-brain
  (:nicknames :brain)
  (:use #:cl
        #:alexandria
        #:iterate)
  (:import-from #:sxql
                #:where)
  (:import-from #:trivia
                #:match
                #:plist)
  (:import-from #:cl-arrows
                #:-<>>)
  (:import-from #:trivial-types
                #:association-list-p)
  (:import-from #:caveman2
                #:defroute
                #:throw-code)
  (:export #:main
           #:setup-db
           #:start-server
           #:stop-server))

(defpackage silver-brain.config
  (:nicknames config conf)
  (:use #:cl
        #:chameleon)
  (:import-from #:find-port
                #:find-port)
  (:export #:app-root
           #:debugp
           #:server-port
           #:server-access-log
           #:database-driver-name
           #:database-file-name
           #:active-profile
           #:set-profile))
