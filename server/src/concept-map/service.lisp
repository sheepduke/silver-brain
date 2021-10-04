(defpackage silver-brain.concept-map.service
  (:use #:cl
        #:silver-brain.util
        #:silver-brain.concept-map.model)
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:serapeum
                #:->
                #:defsubst)
  (:local-nicknames (#:store #:silver-brain.concept-map.store)))

(in-package silver-brain.concept-map.service)

(-> get-concept (string) service-response)
(defun get-concept (uuid)
  (if (not (is-uuid uuid))
      (make-bad-request-response :invalid-uuid)
      (if-let (concept (store:get-concept-by-uuid uuid))
        (make-ok-response concept)
        (make-not-found-response))))
