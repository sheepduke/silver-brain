(defpackage :silver-brain.concept
  (:nicknames :concept)
  (:use :cl)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export #:concept
           #:new
           #:uuid
           #:name
           #:content
           #:parents
           #:children
           #:equals
           #:become-child
           #:become-friend
           #:remove-child
           #:remove-relationship
           #:parentp
           #:childp
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
         :documentation "Randomly generated UUID. This is the unique identifier
 for each concept.")
   (name :accessor name
         :type string
         :initarg :name
         :initform ""
         :documentation "Name of the concept.")
   (content :accessor content
            :type string
            :initarg :content
            :initform ""
            :documentation "Content of the concept.")
   (parents :accessor parents
            :type list
            :initform '()
            :documentation "List of concepts this concept was linked.")
   (children :accessor children
             :type list
             :initform '()
             :documentation "List of concepts this concept links to."))
  (:documentation "A single concept in a network."))

(defun become-child (child concept)
  "Make `child` a child of `concept`."
  (remove-child child concept)
  (setf (children concept)
        (adjoin child (children concept)))
  (setf (parents child)
        (adjoin concept (parents child))))

(defun become-friend (concept friend)
  "Make `concept` and `friend` friends to each other."
  (setf (parents friend) (adjoin concept (parents friend)))
  (setf (children friend) (adjoin concept (children friend)))
  (setf (parents concept) (adjoin friend (parents concept)))
  (setf (children concept) (adjoin friend (children concept))))

(defun remove-relationship  (concept1 concept2)
  "Remove any relationship between `concept1` and `concept2`."
  (remove-child concept1 concept2)
  (remove-child concept2 concept1))

(defun remove-child (child concept)
  "Remove `child` from children of `concept`."
  (setf (children concept) (delete child (children concept)))
  (setf (parents child) (delete concept (parents child))))

(defun childp (child concept)
  "Return `T` if `concept` has a child `child`."
  (and (member child (children concept))
       (not (member child (parents concept)))))

(defun parentp (parent concept)
  "Return `T` if `concept` has a parent `parent`."
  (and (member parent (parents concept))
       (not (member parent (children concept)))))

(defun friendp (concept target)
  "Return `T` if `concept` and `target` are friends."
  (and (member target (parents concept))
       (member target (children concept))))

(defun equals (concept1 concept2)
  "Return `T` if `concept1` is same as `concept2`."
  (string= (uuid concept1) (uuid concept2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Methods                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((concept concept) stream)
  (let ((concept-to-name (lambda (concept) (name concept))))
    (format stream "Concept [~a] Parents {~a} Children {~a}"
            (name concept)
            (mapcar concept-to-name (parents concept))
            (mapcar concept-to-name (children concept)))))
