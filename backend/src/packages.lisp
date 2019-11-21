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
           #:concept-childp #:concept-friendp #:become-child #:become-friend
           #:remove-all-relations-of #:remove-relations-between))

(defpackage silver-brain.db
  (:nicknames db)
  (:use #:cl #:alexandria #:iterate)
  (:import-from #:sxql
                #:where)
  (:export #:setup
           ;; Concept DAO.
           #:concept #:db-concept-to-core-concept
           #:concept-uuid #:concept-name #:concept-content #:concept-content-format
           #:save-concept
           ;; Concept Relation DAO.
           #:concept-relation-source #:concept-relation-target
           ;; Concept.
           #:read-all-concepts #:read-all-concept-relations
           #:find-concept-by-name #:create-concept #:update-concept #:delete-concept
           ;; Concept relation.
           #:add-relation #:delete-relations-of))

(defpackage silver-brain.service
  (:nicknames service)
  (:use #:cl #:alexandria #:iterate #:trivia #:silver-brain.core)
  (:export #:setup
           #:get-all-concepts #:get-concept-by-uuid #:find-concept-by-name
           #:create-concept #:update-concept #:delete-concept
           #:make-child #:make-friend #:remove-relation))

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

(defpackage silver-brain
  (:use #:cl #:alexandria)
  (:export #:main))
