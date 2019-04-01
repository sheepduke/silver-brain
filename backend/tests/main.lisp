(defpackage silver-brain/tests/main
  (:use :cl
        :silver-brain
        :rove))
(in-package :silver-brain/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :silver-brain)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
