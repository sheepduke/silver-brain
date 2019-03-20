(defpackage silver-brain/tests/concept
  (:use :cl
        :silver-brain
        :rove))
(in-package :silver-brain/tests/concept)

(deftest test-concept
  (let ((c1 (concept:new "C1"))
        (c2 (concept:new "C2"))
        (p1 (concept:new "P1"))
        (p2 (concept:new "P2")))
    (testing "Test new."
      (ok (typep c1 'concept:concept))
      (ok (string= (concept:name c1) "C1")
          "Expect name is set correctly."))

    (testing "Test add-child."
      (concept:add-child c1 p1)
      (ok (and (concept:childp c1 p1)
               (concept:parentp p1 c1))
          "Expect add-child works in two-way.")
      (concept:add-child c1 p1)
      (ok (= (length (concept:children c1)) 1)
          "Expect duplicated child will not be added."))

    (concept:add-parent c1 p2)
    (testing "Test add-parent."
      (ok (and (concept:parentp c1 p2)
               (concept:childp p2 c1))
          "Expect add-parent works in two-way."))
    
    (testing "Test add-friend."
      (concept:add-friend c1 c2)
      (ok (and (concept:friendp c1 c2)
               (concept:friendp c2 c1))
          "Expect add-friends works in two-way."))))

(run-test 'test-concept)
