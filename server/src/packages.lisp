(defpackage silver-brain
  (:use #:cl)
  (:export #:main))

(defpackage silver-brain.config
  (:nicknames config)
  (:use #:cl)
  (:export #:active-profile))

(defpackage silver-brain.store
  (:nicknames store)
  (:use #:cl)
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:export #:concept
           #:concept-uuid
           #:concept-name
           #:concept-content-type
           #:concept-content
           #:concept-link
           #:concept-link-uuid
           #:concept-link-source
           #:concept-link-target
           #:object-created-at
           #:object-updated-at           
           #:setup))

(defpackage silver-brain.store.migration
  (:nicknames store.migration)
  (:use #:cl)
  (:export
   #:run-migrations))

(defpackage silver-brain.concept-map
  (:use #:cl))
