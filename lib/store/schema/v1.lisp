(unlisp.prelude:define-package #:silver-brain.store.schema.v1
  (:use #:unlisp.prelude))

(in-package #:silver-brain.store.schema.v1)

(with-auto-export ()
  (defclass concept ()
    ((uuid :col-type (:varchar 64)
           :initarg :uuid
           :initform (format nil "~a" (uuid:make-v4-uuid))
           :reader concept-uuid
           :documentation "The global unique ID of concept. It is a randomly
generated UUID version 4.")
     (name :col-type (:varchar 1024)
           :initarg :name
           :accessor concept-name
           :documentation "The name of concept. Can be any string.")
     (content :col-type (:varchar 1024)
              :initarg :content
              :initform ""
              :accessor concept-content
              :documentation "The content of concept. Imagine it as a Wiki
page.")
     (content-format :col-type (:varchar 16)
                     :initarg :content-format
                     :initform "org"
                     :accessor concept-content-format
                     :documentation "The format of content used by UI."))
    (:metaclass mito:dao-table-class)
    (:documentation "DAO for concept. It only contains the basic information."))

  (defclass concept-relation ()
    ((source :col-type (:varchar 64)
             :initarg :source
             :accessor concept-relation-source)
     (target :col-type (:varchar 64)
             :initarg :target
             :accessor concept-relation-target))
    (:metaclass mito:dao-table-class)
    (:documentation "DAO for relation between concepts. Each relation consists of
SOURCE => TARGET where SOURCE and TARGET are UUID of concepts.")))
