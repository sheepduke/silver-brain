(in-package silver-brain.core)

(defclass concept ()
  ((uuid :col-type (:varchar 64)
         :initarg :uuid
         :reader concept-uuid)
   (name :col-type (:varchar 1024)
         :initarg :name
         :accessor concept-name)
   (content :col-type (:varchar 1024)
            :initarg :content
            :initform ""
            :accessor concept-content)
   (content-format :col-type (:varchar 16)
                   :initarg :content-format
                   :accessor concept-content-format))
  (:metaclass mito:dao-table-class))

(defclass concept-relationship ()
  ((source :col-type concept
           :initarg :source)
   (target :col-type concept
           :initarg :target))
  (:metaclass mito:dao-table-class))

(defun setup-db (&key driver-name database-name)
  "Connect to and setup database."
  (mito:connect-toplevel driver-name :database-name database-name)
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relationship))

(defun add-concept (name content content-format)
  "Add given `concept` to database."
  (let ((concept (make-instance 'concept :uuid (format nil "~a" (make-v4-uuid))
                                         :name name
                                         :content content
                                         :content-format content-format)))
    (mito:save-dao concept)
    concept))

(defun get-concept-by-id (uuid)
  (mito:find-dao 'concept :uuid uuid))

(defun concept-count ()
  (mito:count-dao 'concept))

(defun get-all-concept-id-and-name ()
  "Return a list UUID and name of all concepts in a list of assoc list.
The keys of each alist is `(:id :name)`."
  (mapcar (lambda (concept)
            `((:uuid . ,(concept-uuid concept))
              (:name . ,(concept-name concept))))
          (mito:select-dao 'concept)))

(defun delete-concept-by-id (id)
  )

(defun map-concept ()
  (mito:select-dao 'concept))

;; (get-all-concept-name)
