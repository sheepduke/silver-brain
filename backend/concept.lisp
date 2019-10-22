(defpackage silver-brain.core.concept
  (:nicknames core.concept concept)
  (:use #:cl
        #:alexandria)
  (:export #:concept
           #:uuid #:name #:content-type #:content
           #:parents #:children #:friends
           #:is-parent #:is-friend #:make-parent #:make-friend))

(in-package core.concept)

(defclass concept ()
  ((uuid :type string
         :accessor :uuid
         :initarg :uuid
         :initform (format nil "~a" (uuid:make-v4-uuid))
         :documentation "UUID of the concept.")
   (name :type string
         :accessor name
         :initarg :name
         :documentation "Name (title) of the concept.")
   (content-type :type string
                 :accessor content-type
                 :initarg :content-type
                 :initform ""
                 :documentation "Content type of the concept. This should be
picked up by the UI.")
   (content :type string
            :accessor content
            :initarg :content
            :initform ""
            :documentation "Content of the concept.")
   (parents :type list
            :accessor parents
            :initarg :parents
            :initform '()
            :documentation "The list of references to its parents.")
   (children :type list
             :accessor children
             :initarg :children
             :initform '()
             :documentation "The list of references to its children.")
   (friends :type list
            :accessor friends
            :initarg :friends
            :initform '()
            :documentation "The list of references to its friends.")))

(defmethod print-object ((concept concept) out)
  (format out "#<Concept ~a>" (name concept)))

(defun make-parent (concept parent)
  "Make PARENT a parent of CONCEPT."
  (unless (is-parent concept parent)
    (push parent (parents concept))
    (push concept (children parent))
    (setf (children concept)
          (remove parent (children concept)))
    (setf (parents parent)
          (remove concept (parents parent)))
    (setf (friends concept)
          (remove parent (friends concept)))
    (setf (friends parent)
          (remove concept (friends parent)))))

(defun make-friend (concept friend)
  "Make FRIEND and CONCEPT friends of each other."
  (unless (is-friend concept friend)
    (push friend (friends concept))
    (push concept (friends friend))
    (setf (children concept)
          (remove friend (children concept)))
    (setf (parents concept)
          (remove friend (parents concept)))
    (setf (children friend)
          (remove concept (children friend)))
    (setf (parents friend)
          (remove concept (parents friend)))))

(defun is-parent (concept parent)
  "Return a boolean value indicating if PARENT is a parent of CONCEPT."
  (find parent (parents concept)))

(defun is-friend (concept friend)
  (find friend (friends concept)))

