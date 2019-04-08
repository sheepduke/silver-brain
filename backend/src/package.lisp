(defpackage silver-brain.core
  (:use #:cl
        #:iterate)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export
   ;; concept
   #:concept
   #:concept-id
   #:concept-name
   #:concept-content
   #:concept-content-format
   #:concept-parents
   #:concept-children
   #:concept=
   #:become-child
   #:become-friend
   #:remove-child
   #:remove-relationship
   #:parentp
   #:childp
   #:friendp
   ;; concept-map
   #:concept-map
   #:concept-names
   #:add-concept
   #:concept-count
   #:get-concept-by-id
   #:delete-concept-by-id
   #:map-concept))

(defpackage silver-brain.server
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
