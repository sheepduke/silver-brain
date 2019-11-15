(in-package silver-brain.db)

(defclass concept-relation ()
  ((source :col-type (:varchar 64)
           :initarg :source
           :accessor concept-relation-source)
   (target :col-type (:varchar 64)
           :initarg :target
           :accessor concept-relation-target))
  (:metaclass mito:dao-table-class)
  (:documentation "DAO for relation between concepts. Each relation consists of
SOURCE => TARGET where SOURCE and TARGET are UUID of concepts."))

(defmethod print-object ((relation concept-relation) stream)
  (format stream "#<Relation ~a => ~a>"
          (concept-relation-source relation)
          (concept-relation-target relation)))
