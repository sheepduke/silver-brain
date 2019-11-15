(in-package silver-brain.core)

(defclass concept ()
  ((uuid :type string
         :accessor concept-uuid
         :initarg :uuid
         :initform (format nil "~a" (uuid:make-v4-uuid))
         :documentation "UUID of the concept.")
   (name :type string
         :accessor concept-name
         :initarg :name
         :documentation "Name (title) of the concept.")
   (content-format :type string
                 :accessor concept-content-format
                 :initarg :content-format
                 :initform ""
                 :documentation "Content type of the concept. This should be
picked up by the UI.")
   (content :type string
            :accessor concept-content
            :initarg :content
            :initform ""
            :documentation "Content of the concept.")
   (parents :type list
            :accessor concept-parents
            :initarg :parents
            :initform '()
            :documentation "The list of references to its parents.")
   (children :type list
             :accessor concept-children
             :initarg :children
             :initform '()
             :documentation "The list of references to its children.")
   (friends :type list
            :accessor concept-friends
            :initarg :friends
            :initform '()
            :documentation "The list of references to its friends.")))

(defmethod print-object ((concept concept) out)
  (format out "#<Concept ~a>" (concept-name concept)))

(defun become-child (concept child)
  "Make PARENT a parent of CONCEPT."
  (unless (concept-childp concept child)
    (remove-relations-between concept child)
    (add-child concept child)))

(defun become-friend (concept friend)
  "Make FRIEND and CONCEPT friends of each other."
  (unless (friendp concept friend)
    (remove-relations-between concept friend)
    (add-friend concept friend)))

(defun remove-relations-between (c1 c2)
  "Remove any relations between CONCEPT1 and CONCEPT2."
  (remove-child c1 c2)
  (remove-child c2 c1)
  (remove-friend c1 c2))

(defun add-child (concept child)
  "Let CHILD be a child of CONCEPT, without checking existing relations."
  (unless (concept-childp concept child)
    (adjoin child (concept-children concept))
    (adjoin concept (concept-parents child))))

(defun remove-child (concept child)
  "Remove the relation between CONCEPT and CHILD if CHILD is a child of
CONCEPT."
  (when (concept-childp concept child)
    (setf (concept-children concept)
          (delete child (concept-children concept)))
    (setf (concept-parents child)
          (delete concept (concept-parents child)))))

(defun remove-friend (concept friend)
  "Remove the relation between CONCEPT and FRIEND if they are friends."
  (when (concept-friendp concept friend)
    (setf (concept-friends concept)
          (delete friend (concept-friends concept)))
    (setf (concept-friends friend)
          (delete concept (concept-friends friend)))))

(defun add-friend (concept friend)
  "Let FRIEND be a friend of CONCEPT and vice vesa, without checking existing
relations."
  (unless (concept-friendp concept friend)
    (adjoin friend (concept-friends concept))
    (adjoin concept (concept-friends friend))))

(defun concept-childp (concept child)
  "Return a boolean value indicating if CONCEPT is a child of PARENT."
  (find child (concept-children concept)))

(defun concept-friendp (concept friend)
  "Return a boolean value indicating if FRIEND and CONCEPT are friends."
  (find friend (concept-friends concept)))
