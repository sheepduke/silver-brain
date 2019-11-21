(in-package silver-brain.db)

(defun read-all-concepts ()
  "Return a list UUID and name of all concepts in a list of assoc list.
The keys of each alist is `(:id :name)`."
  (mito:select-dao 'concept))

(defun read-concept-by-uuid (uuid)
  "Get corresponding concept by its UUID."
  (mito:select-dao 'concept
    (where (:= :uuid uuid))))

(defun save-concept (uuid &key name content content-format)
  "Save CONCEPT to database."
  (let ((concept (or (read-concept-by-uuid uuid)
                     (make-instance 'concept :uuid uuid))))
    (setf (concept-name concept) name)
    (setf (concept-content concept) content)
    (setf (concept-content-format concept) content-format)
    (mito:save-dao concept)))

(defun delete-concept (uuid)
  "Delete given CONCEPT from database."
  (mito:delete-by-values 'concept :uuid uuid))
