(defpackage silver-brain.concept-map.store
  (:use #:cl
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.store))
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:->)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache))
  (:export #:get-concept-by-uuid))

(in-package silver-brain.concept-map.store)

(-> get-concept-by-uuid (string) (or null concept))
(defun get-concept-by-uuid (uuid)
  (match (store:get 'store:concept uuid)
    (nil nil)
    ((and (store:concept) concept)
     (make-instance 'concept
                    :uuid (store:uuid concept)
                    :name (store:name concept)
                    :content-type (store:content-type concept)
                    :content (store:content concept)
                    :created-at (store:object-created-at concept)
                    :updated-at (store:object-updated-at concept)))))

;; (setf (silver-brain.config:active-profile) :dev)
;; (store:start)
;; (jsown:to-json (get-concept-by-uuid "5BAAB06F-D70D-4405-8511-3032D12448B3"))
