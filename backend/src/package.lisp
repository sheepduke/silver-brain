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
  (:import-from #:arrow-macros
                #:-<>>
                #:<>)
  (:import-from #:trivial-types
                #:association-list-p)
  (:import-from #:caveman2
                #:defroute
                #:throw-code)
  (:export #:main
           #:set-profile
           #:get-config
           #:set-config
           #:setup-db
           #:start-server
           #:stop-server))
