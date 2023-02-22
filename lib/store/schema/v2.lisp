(unlisp.prelude:defpackage #:silver-brain.store.schema.v2
  (:use #:unlisp.prelude
        #:silver-brain.store.schema.shared)
  (:import-from #:mito.dao.mixin
                #:created-at
                #:updated-at)
  (:export #:created-at
           #:updated-at))

(in-package #:silver-brain.store.schema.v2)

(unlisp.dev:setup-package-local-nicknames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Schema Version                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass meta-info ()
    ((data-version :col-type :text
                   :initarg :data-version
                   :initform (error "Slot DATA-VERSION is unbound")
                   :reader data-version))
    (:metaclass mito:dao-table-class))

  (defmethod io:print-object ((object meta-info) stream)
    (format stream "#<MetaInfo Ver=~a>" (data-version object)))

  (define-clone-object-method meta-info data-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
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
    (:keys name))

  (defmethod io:print-object ((object concept) stream)
    (format stream "#<Concept[~a|~a]>"
            (uuid object)
            (name object)))

  (define-clone-object-method concept uuid name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Concept Alias                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass concept-alias ()
    ((uuid :col-type :text
           :initarg :uuid
           :initform (error "Slot UUID is unbound")
           :reader uuid
           :references (concept uuid))
     (alias :col-type :text
            :initarg :alias
            :initform (error "Slot ALIAS is unbound")
            :reader alias))
    (:metaclass mito:dao-table-class)
    (:keys uuid alias))

  (defmethod io:print-object ((object concept-alias) stream)
    (format stream "#<ConceptAlias[~a|~a]>"
            (uuid object)
            (alias object)))

  (define-clone-object-method concept-alias uuid alias))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Concept Attachment                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
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
    (:metaclass mito:dao-table-class))

  (defmethod io:print-object ((object concept-attachment) stream)
    (format stream "#<ConceptAttachment[~a|~a|~a]>"
            (uuid object)
            (content-type object)
            (if (hyperlink? object) "Hyperlink" "Embedded")))

  (define-clone-object-method concept-attachment uuid content-type content hyperlink?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Concept Relation                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass concept-pair ()
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

  (defmethod initialize-instance :after ((object concept-pair) &rest initargs
                                         &key &allow-other-keys)
    (declare (ignore initargs))
    (with-slots (uuid other) object
      (when (string:>= uuid other)
        (setf (values uuid other)
              (values other uuid)))))

  (defmethod io:print-object ((pair concept-pair) stream)
    (format stream "#<ConceptPair [~a|~a]>"
            (uuid pair)
            (other pair)))

  (define-clone-object-method concept-relation uuid other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Link                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass concept-link ()
    ((source :col-type :text
           :initarg :source
           :initform (error "Slot SOURCE is unbound")
           :reader source
           :references (concept uuid))
     (relation :col-type :text
               :initarg :relation
               :initform (error "Slot RELATION is unbound")
               :reader relation
               :references (concept uuid))
     (target :col-type :text
            :initarg :target
            :initform (error "Slot TARGET is unbound")
            :reader target
            :references (concept uuid)))
    (:metaclass mito:dao-table-class)
    (:keys source target))

  (defmethod io:print-object ((object concept-link) stream)
    (format stream "#<ConceptLink[~a|~a|~a]>"
            (source object)
            (relation object)
            (target object))))
