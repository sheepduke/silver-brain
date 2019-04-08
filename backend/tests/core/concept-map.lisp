(in-package :silver-brain/tests.core)

(deftest concept-map
  (let* ((map (make-instance 'concept-map))
         (software (make-instance 'concept :name "Software"))
         (id (concept-id software))
         (name (concept-name software)))
    (testing "add-concept"
      (add-concept map software)
      (ok (= (hash-table-count (silver-brain.core::concepts map)) 1))
      (ok (string= (concept-name (gethash id (silver-brain.core::concepts map)))
                   name)))

    (testing "get-concept-by-id"
      (let ((result (get-concept-by-id map id)))
        (ok (not (null result)))
        (ok (eq result software))))))
