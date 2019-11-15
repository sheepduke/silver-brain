(defpackage silver-brain.config
  (:nicknames config)
  (:use #:cl)
  (:import-from #:chameleon
                #:defconfig
                #:defprofile)
  (:import-from #:find-port
                #:find-port)
  (:export #:set-profile))

(defpackage silver-brain.core
  (:nicknames core)
  (:use #:cl #:alexandria #:iterate)
  (:export #:concept #:print-object
           ;; Accessors.
           #:concept-uuid #:concept-name #:concept-content-format
           #:concept-content #:concept-parents #:concept-children
           #:concept-friends
           ;; Modifiers.
           #:concept-childp #:concept-friendp #:become-child #:become-friend))

(defpackage silver-brain.db
  (:nicknames db)
  (:use #:cl #:alexandria #:iterate)
  (:import-from #:sxql
                #:where)
  (:export #:setup
           ;; Concept.
           #:concept #:db-concept-to-core-concept
           #:concept-uuid #:concept-name #:concept-content #:concept-content-format
           #:save-concept
           ;; Concept relation.
           #:concept-relation-source #:concept-relation-target
           #:read-all-concepts #:read-all-concept-relations
           #:find-concept-by-name #:create-concept #:update-concept #:delete-concept))

(defpackage silver-brain.service
  (:nicknames service)
  (:use #:cl #:alexandria #:iterate #:trivia #:silver-brain.core)
  (:export #:setup
           #:get-all-concepts #:get-concept-by-uuid #:find-concept-by-name
           #:create-concept #:update-concept #:delete-concept))

(defpackage silver-brain.server
  (:nicknames server)
  (:use #:cl #:alexandria #:core #:cl-arrows #:trivia
        #:silver-brain.core)
  (:import-from #:ningle
                #:*request*
                #:*response*)
  (:import-from #:lack.response
                #:response-status
                #:response-headers
                #:response-body)
  (:import-from #:trivial-types
                #:association-list-p)
  (:export #:start
           #:stop))
