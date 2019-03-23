(defpackage :silver-brain.node
  (:nicknames :node)
  (:use :cl)
  (:import-from :uuid
                #:make-v4-uuid)
  (:export #:node
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
(in-package :silver-brain.node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             CLOS                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node ()
  ((uuid :reader uuid
         :type string
         :initform (format nil "~a" (uuid:make-v4-uuid))
         :documentation "Randomly generated UUID. This is the unique identifier
 for each node.")
   (name :accessor name
         :type string
         :initarg :name
         :initform ""
         :documentation "Name of the node.")
   (content :accessor content
            :type string
            :initarg :content
            :initform ""
            :documentation "Content of the node.")
   (parents :accessor parents
            :type list
            :initform '()
            :documentation "List of nodes this node was linked.")
   (children :accessor children
             :type list
             :initform '()
             :documentation "List of nodes this node links to."))
  (:documentation "A single node in a network."))

(defun new (name)
  "Make a new node of given `name`."
  (make-instance 'node :name name))

(defun become-child (child node)
  "Make `child` a child of `node`."
  (remove-child child node)
  (setf (children node)
        (adjoin child (children node)))
  (setf (parents child)
        (adjoin node (parents child))))

(defun become-friend (node friend)
  "Make `node` and `friend` friends to each other."
  (setf (parents friend) (adjoin node (parents friend)))
  (setf (children friend) (adjoin node (children friend)))
  (setf (parents node) (adjoin friend (parents node)))
  (setf (children node) (adjoin friend (children node))))

(defun remove-relationship  (node1 node2)
  "Remove any relationship between `node1` and `node2`."
  (remove-child node1 node2)
  (remove-child node2 node1))

(defun remove-child (child node)
  "Remove `child` from children of `node`."
  (setf (children node) (delete child (children node)))
  (setf (parents child) (delete node (parents child))))

(defun childp (child node)
  "Return `T` if `node` has a child `child`."
  (and (member child (children node))
       (not (member child (parents node)))))

(defun parentp (parent node)
  "Return `T` if `node` has a parent `parent`."
  (and (member parent (parents node))
       (not (member parent (children node)))))

(defun friendp (node target)
  "Return `T` if `node` and `target` are friends."
  (and (member target (parents node))
       (member target (children node))))

(defun equals (node1 node2)
  "Return `T` if `node1` is same as `node2`."
  (string= (uuid node1) (uuid node2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Methods                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((node node) stream)
  (let ((node-to-name (lambda (node) (name node))))
    (format stream "Node [~a] Parents {~a} Children {~a}"
            (name node)
            (mapcar node-to-name (parents node))
            (mapcar node-to-name (children node)))))
