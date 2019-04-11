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

(defclass concept-relationship ()
  ((source :col-type concept
           :initarg :source
           :accessor concept-relationship-source)
   (target :col-type concept
           :initarg :target
           :accessor concept-relationship-target))
  (:metaclass mito:dao-table-class))

(defun setup-db ()
  "Connect to and setup database."
  (match (get-config :database)
    ((plist :driver-name :sqlite3 :database-name database-name)
     (mito:connect-toplevel :sqlite3 :database-name database-name)))
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relationship))

(defun add-concept (name content content-format)
  "Add given `concept` to database."
  (let ((concept (make-instance 'concept
                                :uuid (format nil "~a" (uuid:make-v4-uuid))
                                :name name
                                :content content
                                :content-format content-format)))
    (mito:save-dao concept)
    concept))

(defun get-concept-by-id (uuid)
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

(defun delete-concept-by-id (uuid)
  (mito:delete-by-values 'concept :uuid uuid))

(defun delete-all-concepts ()
  (mito:delete-by-values 'concept))

(defun become-child (concept child)
  (remove-relationships concept child)
  (mito:insert-dao
   (make-instance 'concept-relationship
                  :source concept
                  :target child)))

(defun remove-relationships (concept1 concept2)
  (mito:delete-by-values 'concept-relationship
                         :source concept1
                         :target concept2))

(defun get-concept-parents (uuid)
  (let ((concept (get-concept-by-id uuid)))
    (mapcar (lambda (relationship)
              (concept-relationship-source relationship))
            (mito:select-dao 'concept-relationship
              (where (:and (:= :target concept)
                           (:!= :source concept)))))))

(defun get-concept-children (uuid)
  (let ((concept (get-concept-by-id uuid)))
    (mapcar (lambda (relationship)
              (concept-relationship-source relationship))
            (mito:select-dao 'concept-relationship
              (where (:and (:= :source concept)
                           (:!= :target concept)))))))

(defun get-concept-friends (uuid)
  (let ((concept (get-concept-by-id uuid)))
    (mapcar (lambda (relationship)
              (concept-relationship-source relationship))
            (mito:select-dao 'concept-relationship
              (where (:and (:= :source concept)
                           (:= :target concept)))))))

(defun linkedp (source target)
  "Return T if there is a link from `source` to `target`."
  (if (mito:select-dao 'concept-relationship
        (where (:and (:= :source source)
                     (:= :target target))))
      t
      nil))
