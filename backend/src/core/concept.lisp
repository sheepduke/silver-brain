(in-package silver-brain)

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

(defmethod print-object ((concept concept) stream)
  (format stream "#<Concept ~a>" (concept-name concept)))

(defun setup-db ()
  "Connect to and setup database."
  (match (get-config :database)
    ((plist :driver-name :sqlite3 :database-name database-name)
     (mito:connect-toplevel :sqlite3 :database-name database-name)))
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))

(defun add-concept (name content content-format)
  "Add given `concept` to database."
  (let ((concept (make-instance 'concept
                                :uuid (format nil "~a" (uuid:make-v4-uuid))
                                :name name
                                :content content
                                :content-format content-format)))
    (mito:save-dao concept)
    concept))

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
  (mito:delete-dao concept))

(defun delete-all-concepts ()
  (mito:delete-by-values 'concept))

