(defpackage silver-brain/tests/concept-map
  (:use #:cl
        #:rove))
(in-package :silver-brain/tests/concept-map)

(deftest concept-map
  (let* ((map (make-instance 'map:concept-map))
         (software (make-instance 'concept:concept :name "Software"))
         (id (concept:id software))
         (name (concept:name software)))
    (testing "add-concept"
      (map:add-concept map software)
      (ok (= (hash-table-count (map:concepts map)) 1))
      (ok (string= (concept:name (gethash id (map:concepts map))) name)))

    (testing "get-concept-by-id"
      (let ((result (map:get-by-id map id)))
        (ok (not (null result)))
        (ok (eq result software))))))
