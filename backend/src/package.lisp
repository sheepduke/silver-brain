(defpackage silver-brain.core
  (:nicknames #:core)
  (:use #:cl
        #:iterate
        #:trivia
        #:mito)
  (:import-from :uuid
                #:make-v4-uuid)
  (:import-from :sxql
                :where)
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
   #:setup-db
   #:add-concept
   #:concept-count
   #:get-concept-by-id
   #:get-all-concepts
   #:find-concepts-by-name
   #:delete-concept-by-id
   #:delete-all-concepts
   #:get-concept-parents
   #:become-child
   #:become-friends
   #:remove-relationships
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
  (:export #:start-server
           #:stop-server))
