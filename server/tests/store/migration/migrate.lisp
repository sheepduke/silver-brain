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
  (let ((database-name (make-random-database-name)))
    (with-database (database-name)
      (migration:run-migrations))

    (with-database (database-name)
      (let ((concepts (mito:select-dao 'store:concept)))
        (is (= 2 (length concepts)))
        (print (store:name (first concepts)))
        (is (find-if (op (string= "Contains" (store:name _))) concepts))
        (is (find-if (op (string= "Relates" (store:name _))) concepts))))
    
    (uiop:delete-file-if-exists database-name)))

(mito:deftable legacy-concept ()
  ((uuid :col-type :string)
   (name :col-type :string)
   (content :col-type :string)
   (content-format :col-type :string))
  (:table-name "concept"))

(mito:deftable legacy-concept-relation ()
  ((source :col-type :string)
   (target :col-type :string))
  (:table-name "concept_relation"))

(test run-migrations-v1-database
  (let ((database-name (make-random-database-name))
        (concept-daos '(("0a" . "Root")
                        ("1a" . "Level 1-1")
                        ("1b" . "Level 1-2")
                        ("2a" . "Level 2-1")
                        ("2b" . "Level 2-2")))
        (concept-relation-daos '(("0a" . "1a")
                                 ("0a" . "1b")
                                 ("1a" . "2a")
                                 ("1a" . "2b")
                                 ("1b" . "2b")
                                 ("2b" . "1b"))))
    (with-database (database-name)
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
    (with-database (database-name)
      (migration:run-migrations))

    (with-database (database-name)
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
        (is (find-if (op (check-concept-link _ contains-uuid "0a" "1a")) links))
        (is (find-if (op (check-concept-link _ contains-uuid "0a" "1b")) links))
        (is (find-if (op (check-concept-link _ contains-uuid "1a" "2a")) links))
        (is (find-if (op (check-concept-link _ contains-uuid "1a" "2b")) links))
        (is (find-if (op (check-concept-link _ relates-uuid "1b" "2b")) links))
        (is (find-if (op (check-concept-link _ relates-uuid "2b" "1b")) links)) ))

    (uiop:delete-file-if-exists database-name)))
