(defpackage silver-brain.concept-map.model
  (:use #:cl
        #:silver-brain.util)
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
           #:relation
           #:target
           #:concept-summary-list
           #:concept-link-list))

(in-package silver-brain.concept-map.model)

(defclass concept ()
  ((uuid :type string
         :accessor uuid
         :initarg :uuid)
   (name :type string
         :accessor name
         :initarg :name
         :initform "")
   (content-type :type string
                 :accessor content-type
                 :initarg :content-type
                 :initform "")
   (content :type string
            :accessor content
            :initarg :content
            :initform "")
   (created-at :type local-time:timestamp
               :accessor created-at
               :initarg :created-at)
   (updated-at :type local-time:timestamp
               :accessor updated-at
               :initarg :updated-at)))

(defmethod jsown:to-json ((obj concept))
  (jsown:to-json (to-json-object obj)))

(defclass concept-summary ()
  ((uuid :type string :accessor uuid :initarg :uuid)
   (name :type string :accessor name :initarg :name)))

(defmethod jsown:to-json ((obj concept-summary))
  (jsown:to-json (to-json-object obj)))

(defun every-concept-summary-p (list)
  (every (op (typep _ 'concept-summary)) list))

(deftype concept-summary-list ()
  `(and list (satisfies every-concept-summary-p)))

(defclass concept-link ()
  ((source :type concept-summary :accessor source :initarg :source)
   (relation :type concept-summary :accessor relation :initarg :relation)
   (target :type concept-summary :accessor target :initarg :target)))

(defun every-concept-link-p (list)
  (every (op (typep _ 'concept-link)) list))

(deftype concept-link-list ()
  `(and list (satisfies every-concept-link-p)))

(defmethod jsown:to-json ((obj concept-link))
  (jsown:to-json (to-json-object obj)))
