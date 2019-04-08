(in-package :silver-brain/tests.core)

(defvar *software* nil)
(defvar *editor* nil)
(defvar *emacs* nil)
(defvar *vim* nil)

(defhook :before
  (setf *software* (make-instance 'concept :name "Software"))
  (setf *editor* (make-instance 'concept :name "Editor"))
  (setf *emacs* (make-instance 'concept :name "Emacs"))
  (setf *vim* (make-instance 'concept :name "Vim"))
  (become-child *software* *editor*)
  (become-child *editor* *emacs*))

(deftest test-concept=
  (ok (concept= *software* *software*)
      "An instance equals to itself.")
  (ok (not (concept= *software* *emacs*))
      "An instance does not equal to someone else."))
    
(deftest test-become-child
  (ok (childp *software* *editor*)
      "The editor is a child of software.")
  (ok (parentp *emacs* *editor*)
      "The emacs is a child of editor."))

(deftest test-remove-child
  (become-child *emacs* *vim*)
  (ok (parentp *vim* *emacs*))
  (remove-child *vim* *emacs*)
  (ok (not (childp *emacs* *vim*))))

(deftest test-become-friend
  (become-friend *emacs* *vim*)
  (ok (not (childp *vim* *emacs*)))
  (ok (not (parentp *vim* *emacs*)))
  (ok (not (parentp *vim* *emacs*)))
  (ok (not (parentp *emacs* *vim*)))
  (ok (friendp *emacs* *vim*)))

(deftest test-concept-children
  (ok (member *emacs* (concept-children *editor*))
      "The emacs is a child of software."))

(deftest test-concept-parents
  (become-child *editor* *emacs*)
  (ok (member *editor* (concept-parents *emacs*))
      "The editor is a parent of emacs."))

