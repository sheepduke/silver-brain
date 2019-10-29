(defpackage silver-brain-tests/db/concept-relation
  (:use #:cl
        #:rove)
  (:import-from #:silver-brain/config)
  (:import-from #:silver-brain/db)
  (:import-from #:silver-brain/db/concept-relation))

(in-package silver-brain-tests/db/concept-relation)

(setup
  (config:set-profile :test)
  (db:setup))

(defvar *parent* nil)
(defparameter +parent-uuid+ "P")
(defparameter +parent-name+ "Parent")

(defvar *child* nil)
(defparameter +child-uuid+ "C")
(defparameter +child-name+ "Child")

(defvar *friend* nil)
(defparameter +friend-uuid+ "F")
(defparameter +friend-name+ "Friend")

(defun setup-test ()
  (db::purge)
  (setf *parent* (make-instance 'db/concept:concept :uuid +parent-uuid+
                                                    :name +parent-name+))
  (setf *child* (make-instance 'db/concept:concept :uuid +child-uuid+
                                                   :name +child-name+))
  (setf *friend* (make-instance 'db/concept:concept :uuid +friend-uuid+
                                                    :name +friend-name+))
  (mito:insert-dao *parent*)
  (mito:insert-dao *child*)
  (mito:insert-dao *friend*)
  (mito:insert-dao (make-instance 'db/concept-relation:concept-relation
                                  :source +parent-uuid+
                                  :target +child-uuid+))
  (mito:insert-dao (make-instance 'db/concept-relation:concept-relation
                                  :source +child-uuid+
                                  :target +friend-uuid+))
  (mito:insert-dao (make-instance 'db/concept-relation:concept-relation
                                  :source +friend-uuid+
                                  :target +child-uuid+)))

(teardown
  (db:disconnect)
  (db::delete-db))

(deftest linkedp
  (setup-test)
  (ok (db/concept-relation:linkedp *parent* *child*)
      "Expect a link from parent to child")
  (ng (db/concept-relation:linkedp *child* *parent*)
      "Expect no link from child to parent")
  (ok (db/concept-relation:linkedp *child* *friend*)
      "Expect a link from child to friend.")
  (ok (db/concept-relation:linkedp *friend* *child*)
      "Expect a link from friend to child."))

(deftest childp
  (setup-test)
  (ok (db/concept-relation:childp *child* *parent*)
      "Child is a child of Parent.")
  (ng (db/concept-relation:childp *child* *friend*)
      "Child is not a child of Friend.")
  (ng (db/concept-relation:childp *friend* *parent*)
      "Friend is not a child of Parent."))

(deftest friendp
  (setup-test)
  (ok (db/concept-relation:friendp *child* *friend*)
      "Child and Friend are friends.")
  (ng (db/concept-relation:friendp *parent* *friend*)
      "Parent and Friend are not friends."))

(deftest get-concept-parents
  (setup-test)
  (let ((parents (db/concept-relation:get-parents-uuid *child*)))
    (ok (= (length parents) 1)
        "Child has only 1 parent.")
    (ok (db/concept:equals (first parents) *parent*)
        "Parent is the parent of Child."))
  (ng (db/concept-relation:get-parents-uuid *parent*)
      "Parent has no parent.")
  (ng (db/concept-relation:get-parents-uuid *friend*)
      "Friend has no parent."))

(deftest get-children-uuid
  (setup-test)
  (let ((children (db/concept-relation:get-children-uuid *parent*)))
    (ok (= (length children) 1)
        "Parent has only 1 child.")
    (ok (db/concept:equals (first children) *child*)
        "Child is a child of Parent."))
  (ng (db/concept-relation:get-children-uuid *child*)
      "Child has no child.")
  (ng (db/concept-relation:get-children-uuid *friend*)
      "Friend has no child."))

(deftest get-friends-uuid
  (setup-test)
  (let ((friends (db/concept-relation:get-friends-uuid *child*)))
    (ok (= (length friends) 1)
        "Child has only 1 friend.")
    (ok (db/concept:equals (first friends) *friend*)
        "Friend is a friend of Child."))
  (let ((friends (db/concept-relation:get-friends-uuid *friend*)))
    (ok (= (length friends) 1)
        "Friend has only 1 friend.")
    (ok (db/concept:equals
         (first (db/concept-relation:get-friends-uuid *friend*)) *child*)))
  (ng (db/concept-relation:get-friends-uuid *parent*)
      "Parent has no friend."))

(deftest become-child
  (setup-test)
  (db/concept-relation:become-child *friend* *parent*)
  (let ((parents (db/concept-relation:get-parents-uuid *friend*)))
    (ok (= (length parents) 1)
        "Friend has only 1 parent.")
    (ok (db/concept:equals *parent* (first parents))
        "Parent is a parent of friend."))
  (let ((children (db/concept-relation:get-children-uuid *parent*)))
    (ok (= (length children) 2)
        "Parent has 2 children.")))

(deftest become-friend
  (setup-test)
  (db/concept-relation:become-friend *parent* *friend*)
  (let ((friends (db/concept-relation:get-friends-uuid *parent*)))
    (ok (= (length friends) 1)
        "Parent has only 1 friend.")
    (ok (db/concept:equals (first friends) *friend*)
        "Friend is a friend of Parent."))
  (let ((friends (db/concept-relation:get-friends-uuid *friend*)))
    (ok (= (length friends) 2)
        "Friend has 2 friends.")))

(deftest remove-child
  (setup-test)
  (db/concept-relation:remove-child *child* *parent*)
  (ng (db/concept-relation:get-children-uuid *parent*)
      "Parent has no child now."))

(deftest remove-friend
  (setup-test)
  (db/concept-relation:remove-friend *child* *friend*)
  (ng (db/concept-relation:friendp *child* *friend*)
      "Child is not a friend of Friend.")
  (ng (db/concept-relation:friendp *friend* *child*)
      "Friend is not a friend of Child."))

(deftest remove-all-relations-of
  (setup-test)
  (db/concept-relation:remove-all-relations-of *child*)
  (ng (db/concept-relation:get-children-uuid *parent*)
      "Parent has no child any more.")
  (ng (db/concept-relation:get-friends-uuid *friend*)
      "Friend has no friend any more."))
