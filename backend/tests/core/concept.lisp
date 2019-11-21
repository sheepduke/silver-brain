(in-package silver-brain-tests.core)

(deftest test-become-child
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (concept-friends b))
    (push b (concept-friends a))
    (become-child b a)
    (testing "B is a parent of A."
      (ok (= (length (concept-parents a)) 1))
      (ok (= (length (concept-children b)) 1))
      (ok (null (concept-children a)))
      (ok (null (concept-parents b)))
      (ok (null (concept-friends a)))
      (ok (null (concept-friends b)))
      (ok (= (length (concept-parents a)) 1))
      (ok (= (length (concept-children b)) 1))
      (ok (eq a (first (concept-children b))))
      (ok (eq b (first (concept-parents a)))))))

(deftest test-become-friend
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (concept-parents b))
    (push b (concept-children a))
    (become-friend a b)
    (testing "A and B are friends."
      (ok (null (concept-parents a)))
      (ok (null (concept-parents b)))
      (ok (null (concept-children a)))
      (ok (null (concept-children b)))
      (ok (= (length (concept-friends a)) 1))
      (ok (= (length (concept-friends b)) 1))
      (ok (eq a (first (concept-friends b))))
      (ok (eq b (first (concept-friends a)))))))

(deftest test-concept-friendp
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no friendship at first."
      (ok (not (concept-friendp a b)))
      (ok (not (concept-friendp b a))))
    (push b (concept-friends a))
    (push a (concept-friends b))
    (testing "A and B are friends after set."
      (ok (concept-friendp a b))
      (ok (concept-friendp b a)))))

(deftest test-concept-childp
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (testing "A and B has no paternity at first."
      (ng (concept-childp b a))
      (ng (concept-childp a b)))
    (push b (concept-parents a))
    (push a (concept-children b))
    (testing "B is a parent of A."
      (ok (concept-childp b a)))))

(deftest test-remove-relations-between
  (let ((a (make-instance 'concept :name "A"))
        (b (make-instance 'concept :name "B")))
    (push a (concept-parents b))
    (push b (concept-children a))
    (testing "A is a parent of B."
      (ok (concept-childp a b)))
    (remove-relations-between a b)
    (testing "A and B have no relations any more."
      (ng (concept-childp a b)))))
