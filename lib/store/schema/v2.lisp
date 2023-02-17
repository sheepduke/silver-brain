(unlisp.prelude:define-package #:silver-brain.store.schema.v2
  (:use #:unlisp.prelude
        #:silver-brain.store.schema.shared))

(in-package #:silver-brain.store.schema.v2)

(unlisp.dev:setup-package-local-nicknames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (export '(uuid name))
  
  (defclass concept ()
    ((uuid :col-type :text
           :initarg :uuid
           :initform (make-uuid)
           :reader uuid)
     (name :col-type :text
           :initarg :name
           :initform ""
           :reader name))
    (:metaclass mito:dao-table-class)
    (:primary-key uuid)
    (:keys name)
    (:auto-pk nil)
    (:record-timestamps nil))

  (defmethod io:print-object ((concept concept) stream)
    (format stream "#Concept[~a|~a]"
            (uuid concept)
            (name concept)))

  (define-clone-object-method concept uuid name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Concept Alias                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (export '(uuid alias))

  (defclass concept-alias ()
    ((uuid :col-type :text
           :initarg :uuid
           :initform (error "Slot UUID is unbound")
           :reader uuid
           :references (concept uuid))
     (alias :col-type :text
            :initarg :uuid
            :initform (error "Slot ALIAS is unbound")
            :reader alias))
    (:metaclass mito:dao-table-class)
    (:keys uuid alias))

  (defmethod io:print-object ((object concept) stream)
    (format stream "#ConceptAlias[~a|~a]"
            (uuid object)
            (alias object)))

  (define-clone-object-method concept-alias uuid alias))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Concept Attachment                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (export '(uuid content-type content hyperlink?))

  (defclass concept-attachment ()
    ((uuid :col-type :text
           :initarg :uuid
           :initform (error "Slot UUID is unbound")
           :reader uuid
           :references (concept uuid))
     (content-type :col-type :text
                   :initarg :content-type
                   :initform (error "Slot CONTENT-TYPE is unbound")
                   :reader content-type)
     (content :col-type :text
              :initarg :content
              :initform (error "Slot CONTENT is unbound")
              :reader content)
     (hyperlink? :col-type :integer
                 :initarg :hyperlink?
                 :initform nil
                 :reader hyperlink?
                 :inflate (op (if (= _ 1) t nil))
                 :deflate (op (if _ 1 0))))
    (:metaclass mito.dao:dao-table-class))

  (defmethod io:print-object ((object concept-attachment) stream)
    (format stream "#ConceptAttachment[~a|~a|~a]"
            (uuid object)
            (content-type object)
            (if (hyperlink? object) "Hyperlink" "Embedded")))

  (define-clone-method concept-attachment uuid content-type content hyperlink?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Concept Relation                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (export '(uuid other))

  (defclass concept-relation ()
    ((uuid :col-type :text
           :initarg :uuid
           :initform (error "Slot UUID is unbound")
           :reader uuid
           :references (concept uuid))
     (other :col-type :text
            :initarg :other
            :initform (error "Slot OTHER is unbound")
            :reader other
            :references (concept uuid)))
    (:metaclass mito:dao-table-class)
    (:keys (uuid other))
    (:documentation "Uuid is always smaller than other by dictionary order."))

  (defmethod initialize-instance :after ((object concept-relation) &rest initargs
                                         &key &allow-other-keys)
    (declare (ignore initargs))
    (with-slots (uuid other) object
      (when (string:>= uuid other)
        (setf (values uuid other)
              (values other uuid)))))

  (defmethod io:print-object ((relation concept-relation) stream)
    (format stream "#Concept Relation[~a|~a]"
            (uuid relation)
            (other relation)))

  (define-clone-object-method concept-relation uuid other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Link                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (export '(left relation right))
  
  (defclass concept-link ()
    ((left :col-type :text
           :initarg :left
           :initform (error "Slot LEFT is unbound")
           :relation left
           :references (concept uuid))
     (relation :col-type :text
               :initarg :relation
               :initform (error "Slot RELATION is unbound")
               :reader relation
               :references (concept uuid))
     (right :col-type :text
            :initarg :right
            :initform (error "Slot RIGHT is unbound")
            :reader right
            :references (concept uuid)))
    (:metaclass mito:dao-table-class)
    (:keys left right))

  (defmethod io:print-object ((object concept-link) stream)
    (format stream "#ConceptLink[~a<~a|~a|~a>]"
            (uuid object)
            (left object)
            (relation object)
            (right object))))
