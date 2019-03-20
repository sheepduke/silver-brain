(defpackage :silver-brain.concept
  (:nicknames :concept)
  (:use :cl)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export #:concept
           #:uuid
           #:name
           #:parents
           #:children
           #:friends
           #:new
           #:equals
           #:add-child
           #:childp
           #:add-parent
           #:parentp
           #:add-friend
           #:friendp
           ))
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
             :documentation "Array of concepts that are pointed by this one.")
   (friends :accessor friends
            :type (vector concept *)
            :initarg :friends
            :initform (make-concept-vector)
            :documentation "Array of concepts that are pointed to each other
with this one."))
  (:documentation "A single node in a concept map."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Public functions                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new (name)
  "Make a new concept of given `name`."
  (make-instance 'concept :name name))

(defun add-child (concept child)
  "Add `child` as a new child of `concept` if it is not already.
Returns `concept`."
  (or (find-if (lambda (x) (equals child x)) (children concept))
      (vector-push-extend child (children concept)))
  (or (find-if (lambda (x) (equals concept x)) (parents child))
      (vector-push-extend concept (parents child)))
  concept)

(defun childp (concept target)
  "Return `T` if `target` is a child of `concept`."
  (if (find target (children concept)) t nil))

(defun parentp (concept target)
  "Return `T` if `target` is a parent of `concept`."
  (if (find target (parents concept)) t nil))

(defun add-parent (concept parent)
  "Add `parent` as a new parent of `concept` if it is not already.
Returns `concept`."
  (add-child parent concept)
  concept)

(defun add-friend (concept friend)
  "Add `friend` as a new friend of `concept` if it is not already.
Returns `concept`."
  (flet ((add-friend-single (c1 c2)
           (or (find-if (lambda (x) (equals c2 x)) (friends c1))
               (vector-push-extend c2 (friends c1)))))
    (add-friend-single concept friend)
    (add-friend-single friend concept))
  concept)

(defun friendp (c1 c2)
  "Return `T` if `c1` and `c2` are friends."
  (if (and (find c1 (friends c2))
           (find c2 (friends c1)))
      t
      nil))

(defun equals (concept1 concept2)
  "Return a boolean value if `concept1` is same as `concept2`."
  (string= (uuid concept1) (uuid concept2)))

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
