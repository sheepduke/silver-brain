(in-package silver-brain.db)

(defclass concept ()
  ((uuid :col-type (:varchar 64)
         :initarg :uuid
         :initform (format nil "~a" (uuid:make-v4-uuid))
         :reader concept-uuid
         :documentation "The global unique ID of concept. It is a randomly
generated UUID version 4.")
   (name :col-type (:varchar 1024)
         :initarg :name
         :accessor concept-name
         :documentation "The name of concept. Can be any string.")
   (content :col-type (:varchar 1024)
            :initarg :content
            :initform ""
            :accessor concept-content
            :documentation "The content of concept. Imagine it as a Wiki
page.")
   (content-format :col-type (:varchar 16)
                   :initarg :content-format
                   :initform "org"
                   :accessor concept-content-format
                   :documentation "The format of content used by UI."))
  (:metaclass mito:dao-table-class)
  (:documentation "DAO for concept. It only contains the basic information."))

(defmethod concept= (c1 c2)
  "Return T if C1 and C2 are equal."
  (and (string= (concept-uuid c1) (concept-uuid c2))
       (string= (concept-name c1) (concept-name c2))
       (string= (concept-content-format c1) (concept-content-format c2))
       (string= (concept-content c1) (concept-content c2))))

(defmethod print-object ((concept concept) stream)
  "Prints the name concept."
  (format stream "#<Concept ~a>" (concept-name concept)))

(defun db-concept-to-core-concept (input)
  "Convert DB:CONCEPT to CORE:CONCEPT."
  (make-instance 'core:concept
                 :uuid (concept-uuid input)
                 :name (concept-name input)
                 :content-format (concept-content-format input)
                 :content (concept-content input)))
