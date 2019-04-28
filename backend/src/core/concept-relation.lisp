(in-package :silver-brain)

(defclass concept-relation ()
  ((source :col-type (:varchar 64)
           :initarg :source
           :accessor concept-relation-source)
   (target :col-type (:varchar 64)
           :initarg :target
           :accessor concept-relation-target))
  (:metaclass mito:dao-table-class))

(defmethod print-object ((relation concept-relation) stream)
  (format stream "#<Relation ~a => ~a>"
          (concept-relation-source relation)
          (concept-relation-target relation)))

(defun linkedp (source target)
  "Return T if there is a link from `source` to `target`."
  (uuid-linkedp (concept-uuid source) (concept-uuid target)))

(defun uuid-linkedp (source-uuid target-uuid)
  "Return T if concepts denoted by given UUID are linked."
  (if (mito:select-dao 'concept-relation
        (where (:and (:= :source source-uuid)
                     (:= :target target-uuid))))
      t
      nil))

(defun get-concept-parents (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :target (concept-uuid concept))))
    (mapcar #'concept-relation-source)
    (remove-if (lambda (uuid) (uuid-linkedp (concept-uuid concept) uuid)))
    (mapcar #'get-concept-by-uuid)))

(defun get-concept-children (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :source (concept-uuid concept))))
    (mapcar #'concept-relation-target)
    (remove-if (lambda (uuid) (uuid-linkedp uuid (concept-uuid concept))))
    (mapcar #'get-concept-by-uuid)))

(defun get-concept-friends (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :source (concept-uuid concept))))
    (mapcar #'concept-relation-target)
    (remove-if-not (lambda (uuid) (uuid-linkedp uuid (concept-uuid concept))))
    (mapcar #'get-concept-by-uuid)))

(defun remove-relations (concept1 concept2)
  (mito:delete-by-values 'concept-relation
                         :source (concept-uuid concept1)
                         :target (concept-uuid concept2))
  (mito:delete-by-values 'concept-relation
                         :source (concept-uuid concept2)
                         :target (concept-uuid concept1)))

(defun become-child (concept child)
  (remove-relations concept child)
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source (concept-uuid concept)
                  :target (concept-uuid child))))

(defun become-friend (concept1 concept2)
  (remove-relations concept1 concept2)
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source (concept-uuid concept1)
                  :target (concept-uuid concept2)))
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source (concept-uuid concept2)
                  :target (concept-uuid concept1))))

(defun childp (concept child)
  (and (linkedp concept child)
       (not (linkedp child concept))))

(defun friendp (concept1 concept2)
  (and (linkedp concept1 concept2)
       (linkedp concept2 concept1)))

(defun remove-child (concept child)
  (when (childp concept child)
    (remove-relations concept child)))

(defun remove-friend (concept1 concept2)
  (when (friendp concept1 concept2)
    (remove-relations concept1 concept2)))

(defun delete-all-concept-relations (concept)
  (mito:delete-by-values 'concept-relation :source (concept-uuid concept))
  (mito:delete-by-values 'concept-relation :target (concept-uuid concept)))
