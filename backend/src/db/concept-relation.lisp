(in-package silver-brain.db)

(defun read-all-concept-relations ()
  "Return all relation objects."
  (mito:select-dao 'concept-relation))

(defun delete-relations-between (uuid1 uuid2)
  "Delete all relations between CONCEPT1 and CONCEPT2."
  (mito:delete-by-values 'concept-relation
                         :source uuid1
                         :target uuid2)
  (mito:delete-by-values 'concept-relation
                         :source uuid2
                         :target uuid1))

(defun delete-relations-of (uuid)
  "Delete all relations related to UUID."
  (mito:delete-by-values 'concept-relation :source uuid)
  (mito:delete-by-values 'concept-relation :target uuid))

(defun add-relation (source-uuid target-uuid)
  "Add a relation from SOURCE-UUID to TARGET-UUID."
  (mito:insert-dao (make-instance 'concept-relation
                                  :source source-uuid
                                  :target target-uuid)))
