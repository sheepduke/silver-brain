(defpackage silver-brain-tests/db/concept
  (:use #:cl #:rove)
  (:import-from silver-brain/db)
  (:import-from silver-brain/db/concept))

(in-package silver-brain-tests/db/concept)

(setup
  (config:set-profile :test)
  (db:setup))

(teardown
  (db:disconnect)
  (db::delete-db))

(defvar *concept* nil)

(defun setup-test ()
  (db::purge)
  (setf *concept* (make-instance 'db/concept:concept :name "Concept A" :uuid "A"))
  (mito:insert-dao *concept*))

(deftest insert
  (setup-test)
  (ok (= (mito:count-dao 'db/concept:concept) 1)
      "There is one concept before adding.")
  (db/concept:insert "B" "" "")
  (ok (= (mito:count-dao 'db/concept:concept) 2)
      "There are two concepts after adding."))

(deftest save
  (setup-test)
  (ok (string= (db/concept:name *concept*) "Concept A")
      "Old name is \"Concept A\".")
  (setf (db/concept:name *concept*) "B")
  (db/concept:save *concept*)
  (let* ((new-concept (first (mito:select-dao 'db/concept:concept))))
    (ok (string= (db/concept:name new-concept) "B")
        "New name is \"B\".")))

(deftest get-by-uuid
  (setup-test)
  (let* ((got (db/concept:get-by-uuid "A")))
    (ok (db/concept:equals got *concept*)
        "Returns expected item."))
  (let* ((got (db/concept:get-by-uuid "Wrong")))
    (ok (null got)
        "Returns NIL with wrong UUID.")))

(deftest get-all
  (setup-test)
  (let* ((got (db/concept:get-all)))
    (ok (= (length got) 1)
        "Returns correct count of items.")
    (ok (db/concept:equals (first got) *concept*)
        "Returns correct item.")))

(deftest find-by-name
  (setup-test)
  (let* ((got (db/concept:find-by-name "con")))
    (ok (= (length got) 1)
        "Returns correct count of items.")
    (ok (db/concept:equals (first got) *concept*)
        "Returns correct item.")
    (ok (null (db/concept:find-by-name "hello"))
        "Returns empty when search condition is wrong.")))

(deftest erase
  (setup-test)
  (ok (= (mito:count-dao 'db/concept:concept) 1)
      "There is one item in the database.")
  (db/concept:erase *concept*)
  (ok (= (mito:count-dao 'db/concept:concept) 0)
      "There is no item after erasing."))
