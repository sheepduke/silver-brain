(unlisp:defpackage #:silver-brain.store.schema.v1
  (:use #:unlisp
        #:silver-brain.store.schema.util)
  (:import-from #:mito.dao.mixin
                #:id
                #:created-at
                #:updated-at))

(in-package #:silver-brain.store.schema.v1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (def schema-version "V1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Meta Info                           ;;;;
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

  (define-clone-object-method meta-info (id data-version created-at)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass concept ()
    ((uuid :col-type (:varchar 64)
           :initarg :uuid
           :initform (make-uuid)
           :reader uuid
           :documentation "The global unique ID of concept. It is a randomly
generated UUID version 4.")
     (name :col-type (:varchar 1024)
           :initarg :name
           :accessor name
           :documentation "The name of concept. Can be any string.")
     (content :col-type (:varchar 1024)
              :initarg :content
              :initform ""
              :accessor content
              :documentation "The content of concept. Imagine it as a Wiki
page.")
     (content-format :col-type (:varchar 16)
                     :initarg :content-format
                     :initform "org"
                     :accessor content-format
                     :documentation "The format of content used by UI."))
    (:metaclass mito:dao-table-class)
    (:documentation "DAO for concept. It only contains the basic information."))

  (define-object-equal-method concept (uuid name content content-format created-at)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Concept Relation                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defclass concept-relation ()
    ((source :col-type (:varchar 64)
             :initarg :source
             :accessor source)
     (target :col-type (:varchar 64)
             :initarg :target
             :accessor target))
    (:metaclass mito:dao-table-class)
    (:documentation "DAO for relation between concepts. Each relation consists of
SOURCE => TARGET where SOURCE and TARGET are UUID of concepts."))

  (define-object-equal-method concept-relation (id source target created-at)))
