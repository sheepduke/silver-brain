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
           #:links
           #:create-time
           #:update-time
           #:concept-summary
           #:concept-link
           #:source
           #:relation
           #:target
           #:concept-summary-list
           #:concept-link-list))

(in-package silver-brain.concept-map.model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Classes                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass concept-summary ()
  ((uuid :type string :accessor uuid :initarg :uuid)
   (name :type string :accessor name :initarg :name)))

(defun every-concept-summary-p (list)
  (every (op (typep _ 'concept-summary)) list))

(deftype concept-summary-list ()
  `(and list (satisfies every-concept-summary-p)))

(defclass concept-link ()
  ((uuid :type string :accessor uuid :initarg :uuid)
   (source :type concept-summary :accessor source :initarg :source)
   (relation :type concept-summary :accessor relation :initarg :relation)
   (target :type concept-summary :accessor target :initarg :target)
   (directionalp :type boolean
                 :accessor directional
                 :initarg :directionalp
                 :json-key-name "is-directional"))
  (:metaclass json-serializable-class))

(defun every-concept-link-p (list)
  (every (op (typep _ 'concept-link)) list))

(deftype concept-link-list ()
  `(and list (satisfies every-concept-link-p)))

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
   (links :type concept-link-list
          :accessor links
          :initarg :links
          :initform '())
   (create-time :type local-time:timestamp
               :accessor create-time
               :initarg :create-time)
   (update-time :type local-time:timestamp
               :accessor update-time
               :initarg :update-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Methods                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod jsown:to-json ((obj concept))
  (jsown:to-json (to-json-object obj)))

(defmethod jsown:to-json ((obj concept-summary))
  (jsown:to-json (to-json-object obj)))

(defmethod jsown:to-json ((obj concept-link))
  (jsown:to-json (to-json-object obj)))
