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
                   :type string
                   :initform ""
                   :documentation "Format of content.")
   (parents :accessor concept-parents
            :type list
            :initform '()
            :documentation "List of concepts this concept was linked.")
   (children :accessor concept-children
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
  (setf (concept-children concept)
        (adjoin child (concept-children concept)))
  (setf (concept-parents child)
        (adjoin concept (concept-parents child))))

(defun become-friend (concept friend)
  "Make `concept` and `friend` friends to each other."
  (setf (concept-parents friend) (adjoin concept (concept-parents friend)))
  (setf (concept-children friend) (adjoin concept (concept-children friend)))
  (setf (concept-parents concept) (adjoin friend (concept-parents concept)))
  (setf (concept-children concept) (adjoin friend (concept-children concept))))

(defun remove-relationship  (concept1 concept2)
  "Remove any relationship between `concept1` and `concept2`."
  (remove-child concept1 concept2)
  (remove-child concept2 concept1))

(defun remove-child (child concept)
  "Remove `child` from children of `concept`."
  (setf (concept-children concept) (delete child (concept-children concept)))
  (setf (concept-parents child) (delete concept (concept-parents child))))

(defun childp (child concept)
  "Return `T` if `concept` has a child `child`."
  (and (member child (concept-children concept))
       (not (member child (concept-parents concept)))))

(defun parentp (parent concept)
  "Return `T` if `concept` has a parent `parent`."
  (and (member parent (concept-parents concept))
       (not (member parent (concept-children concept)))))

(defun friendp (concept target)
  "Return `T` if `concept` and `target` are friends."
  (and (member target (concept-parents concept))
       (member target (concept-children concept))))

(defmethod print-object ((concept concept) stream)
  (let ((concept-to-name (lambda (concept) (concept-name concept))))
    (format stream "Concept [~a] Parents {~a} Children {~a}"
            (concept-name concept)
            (mapcar concept-to-name (concept-parents concept))
            (mapcar concept-to-name (concept-children concept)))))
