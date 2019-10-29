(defpackage silver-brain/db/concept-relation-dao
  (:nicknames db/concept-relation-dao)
  (:use #:cl)
  (:export #:concept-relation #:print-object
           #:source #:target))
(in-package silver-brain/db/concept-relation-dao)

(defclass concept-relation ()
  ((source :col-type (:varchar 64)
           :initarg :source
           :accessor source)
   (target :col-type (:varchar 64)
           :initarg :target
           :accessor target))
  (:metaclass mito:dao-table-class))

(defmethod print-object ((relation concept-relation) stream)
  (format stream "#<Relation ~a => ~a>"
          (concept-relation-source relation)
          (concept-relation-target relation)))
