(in-package :silver-brain.core)

(defclass concept ()
  ((id :reader concept-id
       :type string
       :initform (format nil "~a" (make-v4-uuid))
       :documentation "Randomly generated ID. This is the unique identifier
 for each concept.")
   (name :accessor concept-name
         :type string
         :initarg :name
         :initform ""
         :documentation "Name of the concept.")
   (content :accessor concept-content
            :type string
            :initarg :content
            :initform ""
            :documentation "Content of the concept.")
   (content-format :accessor concept-content-format
                   :type (member :org :markdown :plain)
                   :initarg :content-format
                   :initform :org
                   :documentation "Format of content.")
   (parents :accessor parents
            :type list
            :initform '()
            :documentation "List of concepts this concept was linked.")
   (children :accessor children
             :type list
             :initform '()
             :documentation "List of concepts this concept links to."))
  (:documentation "A single concept in a network."))

(defun concept= (concept1 concept2)
  "Return `T` if `concept1` is same as `concept2`."
  (string= (concept-id concept1) (concept-id concept2)))

(defun become-child (concept child)
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

(defun concept-children (concept)
  "Return all children of `concept` as a list."
  (remove-if-not (lambda (child)
                   (childp concept child))
                 (children concept)))

(defun concept-parents (concept)
  "Return all parents of `concept` as a list."
  (remove-if-not (lambda (parent)
                   (parentp concept parent))
                 (parents concept)))

(defun childp (concept child)
  "Return `T` if `concept` has a child `child`."
  (and (member child (children concept))
       (not (member child (parents concept)))))

(defun parentp (concept parent)
  "Return `T` if `concept` has a parent `parent`."
  (and (member parent (parents concept))
       (not (member parent (children concept)))))

(defun friendp (concept target)
  "Return `T` if `concept` and `target` are friends."
  (and (member target (parents concept))
       (member target (children concept))))

(defmethod print-object ((concept concept) stream)
  (let ((concept-to-name (lambda (concept) (concept-name concept))))
    (format stream "Concept [~a] Parents {~a} Children {~a}"
            (concept-name concept)
            (mapcar concept-to-name (parents concept))
            (mapcar concept-to-name (children concept)))))
