(defpackage :silver-brain.concept
  (:nicknames :concept)
  (:use :cl)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export #:concept
           #:uuid
           #:name
           #:parents
           #:children))
(in-package :silver-brain.concept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             CLOS                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass concept ()
  ((uuid :reader uuid
       :type string
       :initform (format nil "~a" (uuid:make-v4-uuid))
       :documentation "Randomly generated UUID.")
   (name :accessor name
         :type string
         :initarg :name
         :initform "")
   (parents :accessor parents
            :type (vector concept *)
            :initarg :parent
            :initform (make-concept-vector)
            :documentation "Array of concepts that points to the current one.")
   (children :accessor children
             :type (vector concept *)
             :initarg :children
             :initform (make-concept-vector)
             :documentation "Array of concepts that are pointed by this one. "))
  (:documentation "A single node in a concept map."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Public functions                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-child (concept child-concept)
  "Add `child-concept` as a new child of `concept` if it is not already.
Returns `concept`."
  ;; (when )
  (vector-push-extend child-concept (children concept)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Private functions                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-concept-vector ()
  "Return an adjustable vector that holds Concept instances."
  (make-array 0 :element-type 'concept
                :adjustable t
                :fill-pointer t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Methods                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((concept concept) stream)
  (format stream "Concept [~a] <~a>" (name concept) (uuid concept)))
