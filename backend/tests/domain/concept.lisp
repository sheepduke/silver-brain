(defpackage silver-brain-tests/domain/concept
  (:use #:cl
        #:rove
        #:silver-brain/domain/concept))

(in-package silver-brain-tests/domain/concept)

(deftest test-become-child
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (friends b))
    (push b (friends a))
    (become-child a b)
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

(deftest test-become-friend
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (parents b))
    (push b (children a))
    (become-friend a b)
    (testing "A and B are friends."
      (ok (null (parents a)))
      (ok (null (parents b)))
      (ok (null (children a)))
      (ok (null (children b)))
      (ok (= (length (friends a)) 1))
      (ok (= (length (friends b)) 1))
      (ok (eq a (first (friends b))))
      (ok (eq b (first (friends a)))))))

(deftest test-friendp
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no friendship at first."
      (ok (not (friendp a b)))
      (ok (not (friendp b a))))
    (push b (friends a))
    (push a (friends b))
    (testing "A and B are friends after set."
      (ok (friendp a b))
      (ok (friendp b a)))))

(deftest test-childp
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no paternity at first."
      (ok (not (childp b a)))
      (ok (not (childp a b))))
    (push b (parents a))
    (push a (children b))
    (testing "B is a parent of A."
      (ok (childp a b)))))
