(in-package :silver-brain)

(defclass concept-relation ()
  ((source :col-type concept
           :initarg :source
           :accessor concept-relation-source)
   (target :col-type concept
           :initarg :target
           :accessor concept-relation-target))
  (:metaclass mito:dao-table-class))

(defmethod print-object ((relation concept-relation) stream)
  (format stream "#<Relation ~a => ~a>"
          (concept-name (concept-relation-source relation))
          (concept-name (concept-relation-target relation))))

(defun linkedp (source target)
  "Return T if there is a link from `source` to `target`."
  (if (mito:select-dao 'concept-relation
        (where (:and (:= :source source)
                     (:= :target target))))
      t
      nil))

(defun get-concept-parents (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :target concept)))
    (remove-if (lambda (r) (linkedp concept (concept-relation-source r))))
    (mapcar (lambda (r) (concept-relation-source r)))))

(defun get-concept-children (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :source concept)))
    (remove-if (lambda (r) (linkedp (concept-relation-target r) concept)))
    (mapcar (lambda (r) (concept-relation-target r)))))

(defun get-concept-friends (concept)
  (-<>> (mito:select-dao 'concept-relation
          (where (:= :source concept)))
    (remove-if-not (lambda (r) (linkedp (concept-relation-target r) concept)))
    (mapcar (lambda (r) (concept-relation-target r)))))

(defun remove-relations (concept1 concept2)
  (mito:delete-by-values 'concept-relation
                         :source concept1
                         :target concept2)
  (mito:delete-by-values 'concept-relation
                         :source concept2
                         :target concept1))

(defun become-child (concept child)
  (remove-relations concept child)
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source concept
                  :target child)))

(defun become-friend (concept1 concept2)
  (remove-relations concept1 concept2)
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source concept1
                  :target concept2))
  (mito:insert-dao
   (make-instance 'concept-relation
                  :source concept2
                  :target concept1)))

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
