(unlisp.prelude:defpackage #:silver-brain.store.schema.v2
  (:use #:unlisp.prelude
        #:silver-brain.store.schema.util)
  (:import-from #:mito.dao.mixin
                #:created-at
                #:updated-at))

(in-package #:silver-brain.store.schema.v2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (def schema-version "V2"))

(reexport-from #:silver-brain.store.schema.v1
  #:meta-info #:data-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Concept                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(concept uuid name))
(mito:deftable concept ()
  ((uuid :col-type :text
         :initform (make-uuid))
   (name :col-type :text
         :initform (error "Slot NAME is unbound")))
  (:primary-key uuid)
  (:keys name)
  (:conc-name ""))

(defmethod io:print-object ((object concept) stream)
  (format stream "#<Concept[~a|~a]>"
          (uuid object)
          (name object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Concept Alias                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(concept-alias concept alias))
(mito:deftable concept-alias ()
  ((concept :col-type concept)
   (alias :col-type :text
          :initform (error "Slot ALIAS is unbound")))
  (:keys concept alias)
  (:conc-name ""))

(defmethod io:print-object ((object concept-alias) stream)
  (format stream "#<ConceptAlias[~a|~a]>"
          (concept object)
          (alias object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Concept Attachment                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(concept-attachment concept concept-uuid content-type content-length))
(mito:deftable concept-attachment ()
  ((concept :col-type concept)
   (name :col-type :text
         :initform "Body")
   (content-type :col-type :text
                 :initform (error "Slot CONTENT-TYPE is unbound"))
   (content-length :col-type :integer
                   :initform (error "Slot CONTENT-LENGTH is unbound")))
  (:conc-name ""))

(defmethod io:print-object ((object concept-attachment) stream)
  (format stream "#<ConceptAttachment[~a|~a|~a|~a]>"
          (concept object)
          (name object)
          (content-type object)
          (if (hyperlink? object) "Hyperlink" "Embedded")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Pair                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(concept-pair concept concept-uuid other other-uuid))
(mito:deftable concept-pair ()
  ((concept :col-type concept)
   (other :col-type concept))
  (:keys (concept other))
  (:conc-name "")
  (:documentation "Uuid is always smaller than other by dictionary order."))

(defmethod io:print-object ((pair concept-pair) stream)
  (format stream "#<ConceptPair [~a|~a]>" (concept pair) (other pair)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Concept Link                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(concept-link source source-uuid relation relation-uuid
          target target-uuid))
(mito:deftable concept-link ()
  ((source :col-type concept)
   (relation :col-type concept)
   (target :col-type concept))
  (:keys source target)
  (:conc-name ""))

(defmethod io:print-object ((object concept-link) stream)
  (format stream "#<ConceptLink[~a|~a|~a]>"
          (source object)
          (relation object)
          (target object)))
