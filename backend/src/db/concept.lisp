(defpackage silver-brain/db/concept
  (:nicknames db/concept)
  (:use #:cl)
  (:export #:concept
           #:uuid #:name #:content #:content-format
           #:print-object #:add-concept #:save))

(in-package silver-brain/db/concept)

(defclass concept ()
  ((uuid :col-type (:varchar 64)
         :initarg :uuid
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
                   :accessor concept-content-format
                   :documentation "The format of content used by UI."))
  (:metaclass mito:dao-table-class))

(defmethod print-object ((concept concept) stream)
  (format stream "#<Concept ~a>" (concept-name concept)))

(defun add-concept (name content content-format)
  "Add given `concept` to database."
  (let ((concept (make-instance 'concept
                                :uuid (format nil "~a" (uuid:make-v4-uuid))
                                :name name
                                :content content
                                :content-format content-format)))
    (mito:save-dao concept)
    concept))

(defun save-concept (concept)
  "Save `concept` to database."
  (mito:save-dao concept))

(defun get-concept-by-uuid (uuid)
  (mito:find-dao 'concept :uuid uuid))

(defun concept-count ()
  (mito:count-dao 'concept))

(defun get-all-concepts ()
  "Return a list UUID and name of all concepts in a list of assoc list.
The keys of each alist is `(:id :name)`."
  (mito:select-dao 'concept))

(defun find-concepts-by-name (search)
  (mito:select-dao 'concept
    (where (:like :name (format nil "%~a%" search)))))

(defun delete-concept (concept)
  (mito:delete-dao concept)
  (delete-all-concept-relations concept))

(defun delete-all-concepts ()
  (mito:delete-by-values 'concept)
  (mito:delete-by-values 'concept-relation))
