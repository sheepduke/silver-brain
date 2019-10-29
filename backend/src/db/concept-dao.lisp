(defpackage silver-brain/db/concept-dao
  (:nicknames db/concept-dao)
  (:use #:cl)
  (:export #:concept #:uuid #:name #:content #:content-format
           #:equals #:print-object))
(in-package silver-brain/db/concept-dao)

(defclass concept ()
  ((uuid :col-type (:varchar 64)
         :initarg :uuid
         :initform (format nil "~a" (uuid:make-v4-uuid))
         :reader uuid
         :documentation "The global unique ID of concept. It is a randomly
generated UUID version 4.")
   (name :col-type (:varchar 1024)
         :initarg :name
         :accessor name
         :documentation "The name of concept. Can be any string.")
   (content :col-type (:varchar 1024)
            :initarg :content
            :initform ""
            :accessor content
            :documentation "The content of concept. Imagine it as a Wiki
page.")
   (content-format :col-type (:varchar 16)
                   :initarg :content-format
                   :initform "org"
                   :accessor content-format
                   :documentation "The format of content used by UI."))
  (:metaclass mito:dao-table-class)
  (:documentation "DAO for concept. It only contains the basic information."))

(defmethod print-object ((concept concept) stream)
  "Prints the name concept."
  (format stream "#<Concept ~a>" (name concept)))

(defun equals (c1 c2)
  "Return T if C1 and C2 are equal."
  (and (string= (uuid c1) (uuid c2))
       (string= (name c1) (name c2))
       (string= (content-format c1) (content-format c2))
       (string= (content c1) (content c2))))
