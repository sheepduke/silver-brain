(defpackage silver-brain/tests.core.concept
  (:use #:cl
        #:rove
        #:silver-brain.core.concept))

(in-package silver-brain/tests.core.concept)

(deftest test-make-parent
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (friends b))
    (push b (friends a))
    (make-parent a b)
    (testing "B is a parent of A."
      (ok (= (length (parents a)) 1))
      (ok (= (length (children b)) 1))
      (ok (null (children a)))
      (ok (null (parents b)))
      (ok (null (friends a)))
      (ok (null (friends b)))
      (ok (= (length (parents a)) 1))
      (ok (= (length (children b)) 1))
      (ok (eq a (first (children b))))
      (ok (eq b (first (parents a)))))))

(deftest test-make-friend
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (parents b))
    (push b (children a))
    (make-friend a b)
    (testing "A and B are friends."
      (ok (null (parents a)))
      (ok (null (parents b)))
      (ok (null (children a)))
      (ok (null (children b)))
      (ok (= (length (friends a)) 1))
      (ok (= (length (friends b)) 1))
      (ok (eq a (first (friends b))))
      (ok (eq b (first (friends a)))))))

(deftest test-is-friend
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no friendship at first."
      (ok (not (is-friend a b)))
      (ok (not (is-friend b a))))
    (push b (friends a))
    (push a (friends b))
    (testing "A and B are friends after set."
      (ok (is-friend a b))
      (ok (is-friend b a)))))

(deftest test-is-parent
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no paternity at first."
      (ok (not (is-parent b a)))
      (ok (not (is-parent a b))))
    (push b (parents a))
    (push a (children b))
    (testing "B is a parent of A."
      (ok (is-parent a b)))))
