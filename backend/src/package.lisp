(defpackage silver-brain.core
  (:nicknames #:core)
  (:use #:cl
        #:iterate
        #:trivia
        #:mito)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export
   ;; concept
   #:concept
   #:concept-uuid
   #:concept-name
   #:concept-content
   #:concept-content-format
   ;; #:concept-parents
   ;; #:concept-children
   ;; #:concept-friends
   ;; #:become-child
   ;; #:become-friend
   ;; #:remove-child
   ;; #:remove-relationship
   ;; #:parentp
   ;; #:childp
   ;; #:friendp
   #:add-concept
   #:concept-count
   #:get-concept-by-id
   #:delete-concept-by-id
   #:get-all-concept-id-and-name
   #:setup-db
   ;; config
   #:set-profile
   #:get-config
   #:get-database-spec
   ))

(defpackage silver-brain.server
  (:nicknames :server)
  (:use #:cl
        #:alexandria
        #:iterate
        #:trivia
        #:silver-brain.core)
  (:import-from #:caveman2
                #:*request*
                #:*response*
                #:response-status
                #:response-headers
                #:defroute
                #:throw-code)
  (:import-from #:cl-json
                #:decode-json-from-string
                #:encode-json-to-string)
  (:import-from #:trivial-types
                #:association-list-p)
  (:export #:setup-server
           #:start-server
           #:stop-server))
