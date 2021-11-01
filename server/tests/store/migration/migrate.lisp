(defpackage silver-brain-tests.store.migration
  (:use #:cl
        #:silver-brain-tests)
  (:local-nicknames (#:migration #:silver-brain.store.migration)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:test
                #:is
                #:def-suite*)
  (:import-from #:serapeum
                #:op))

(in-package silver-brain-tests.store.migration)

(def-suite* silver-brain.store.migration :in silver-brain)

(defun check-concept-link (link uuid source target)
  (and (string= uuid (store:uuid link))
       (string= source (store:source link))
       (string= target (store:target link))))

(test run-migrations-empty-database
  (setf (silver-brain.config:active-profile) :test)
  (with-random-database-file
    (store:with-current-database
      (migration:run-migrations))

    (store:with-current-database
      (let ((concepts (mito:select-dao 'store:concept)))
        (is (= 2 (length concepts)))
        (print (store:name (first concepts)))
        (is (find-if (op (string= "Contains" (store:name _))) concepts))
        (is (find-if (op (string= "Relates" (store:name _))) concepts))))))

(mito:deftable legacy-concept ()
  ((uuid :col-type :text)
   (name :col-type :text)
   (content :col-type :text)
   (content-format :col-type :text))
  (:table-name "concept"))

(mito:deftable legacy-concept-relation ()
  ((source :col-type :text)
   (target :col-type :text))
  (:table-name "concept_relation"))

(test run-migrations-v1-database
  (setf (silver-brain.config:active-profile) :test)
  (let ((concept-daos '(("0" . "Root")
                        ("11" . "Level 1-1")
                        ("12" . "Level 1-2")
                        ("21" . "Level 2-1")
                        ("22" . "Level 2-2")))
        (concept-relation-daos '(("0" . "11")
                                 ("0" . "12")
                                 ("11" . "21")
                                 ("11" . "22")
                                 ("12" . "22")
                                 ("22" . "12"))))
    (with-random-database-file
      (store:with-current-database
        (mito:ensure-table-exists 'legacy-concept)
        (mito:ensure-table-exists 'legacy-concept-relation)

        ;; Construct a tree like:
        ;;         0
        ;;       /   \
        ;;    1-1     1-2
        ;;    / \      |
        ;;  2-1  2-2 --+
        (dolist (pair concept-daos)
          (mito:insert-dao (make-instance 'legacy-concept
                                          :uuid (car pair)
                                          :name (cdr pair)
                                          :content-format ""
                                          :content "")))
        (dolist (pair concept-relation-daos)
          (mito:insert-dao (make-instance 'legacy-concept-relation
                                          :source (car pair)
                                          :target (cdr pair)))))
    
      ;; Run.
      (store:with-current-database
        (migration:run-migrations))

      (store:with-current-database
        (let* ((concepts (mito:select-dao 'store:concept))
               (links (mito:select-dao 'store:concept-link))
               (contains (find-if (op (string= "Contains" (store:name _)))
                                  concepts))
               (contains-uuid (store:uuid contains))
               (relates (find-if (op (string= "Relates" (store:name _)))
                                 concepts))
               (relates-uuid (store:uuid relates)))
          ;; New concepts equal to the original ones.
          (is (equal concept-daos
                     (subseq (mapcar (lambda (concept)
                                       (cons (store:uuid concept)
                                             (store:name concept)))
                                     concepts)
                             0 5)))
          ;; 2 special concepts are inserted.
          (is (not (null contains)))
          (is (not (null relates)))
          ;; Links are inserted.
          (is (= 6 (length links)))
          (is (find-if (op (check-concept-link _ contains-uuid "0" "11")) links))
          (is (find-if (op (check-concept-link _ contains-uuid "0" "12")) links))
          (is (find-if (op (check-concept-link _ contains-uuid "11" "21")) links))
          (is (find-if (op (check-concept-link _ contains-uuid "11" "22")) links))
          (is (find-if (op (check-concept-link _ relates-uuid "12" "22")) links))
          (is (find-if (op (check-concept-link _ relates-uuid "22" "12")) links)) )))))
