(defpackage silver-brain.concept-map.model
  (:use #:cl)
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:op
                #:~>>)
  (:export #:concept
           #:uuid
           #:name
           #:content-type
           #:content
           #:created-at
           #:updated-at
           #:concept-summary
           #:concept-link
           #:source
           #:target))

(in-package silver-brain.concept-map.model)

(defclass concept ()
  ((uuid :type string :accessor uuid :initarg :uuid)
   (name :type string :accessor name :initarg :name)
   (content-type :type string :accessor content-type :initarg :content-type)
   (content :type string :accessor content :initarg :content)
   (created-at :type local-time:timestamp
               :accessor created-at
               :initarg :created-at)
   (updated-at :type local-time:timestamp
               :accessor updated-at
               :initarg :updated-at)))

(defclass concept-summary ()
  ((uuid :type string :accessor uuid :initarg :uuid)
   (name :type string :accessor name :initarg :name)))

(defclass concept-link ()
  ((source :type concept-summary :accessor source :initarg :source)
   (target :type concept-summary :accessor target :initarg :target)))
