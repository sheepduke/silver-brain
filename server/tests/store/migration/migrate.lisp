(defpackage silver-brain-tests.store.migration
  (:use #:cl
        #:silver-brain-tests)
  (:local-nicknames (#:migration #:silver-brain.store.migration)
                    (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:test
                #:is
                #:def-suite*)
  (:import-from #:serapeum
                #:op))

(in-package silver-brain-tests.store.migration)

(def-suite* silver-brain.store.migration :in silver-brain)

(defun check-concept-link (link source relation target)
  (and (string= source (store:source link))
       (string= relation (store:relation link))
       (string= target (store:target link))))

(test run-migrations-empty-database
  (config:with-profile :test
    (with-random-database-file (:connectp nil)
      (store:with-current-database
        (migration:run-migrations))

      (store:with-current-database
        (let ((concepts (mito:select-dao 'store:concept)))
          (is (= 2 (length concepts)))
          (is (not (null (find-if (op (string= "Contains" (store:name _)))
                                  concepts))))
          (is (not (null (find-if (op (string= "Relates" (store:name _)))
                                  concepts)))))))))

(mito:deftable legacy-concept ()
  ((uuid :col-type :text)
   (name :col-type :text)
   (content :col-type :text)
   (content-format :col-type :text)
   (created-at :col-type :timestamp)
   (updated-at :col-type :timestamp))
  (:table-name "concept")
  (:record-timestamps nil))

(mito:deftable legacy-concept-relation ()
  ((source :col-type :text)
   (target :col-type :text))
  (:table-name "concept_relation"))

(test run-migrations-v1-database
  (config:with-profile :test
    (let* ((concept-daos '(("0" . "Root")
                           ("11" . "Level 1-1")
                           ("12" . "Level 1-2")
                           ("21" . "Level 2-1")
                           ("22" . "Level 2-2")))
           (concept-relation-daos '(("0" . "11")
                                    ("0" . "12")
                                    ("11" . "21")
                                    ("11" . "22")
                                    ("12" . "22")
                                    ("22" . "12")))
           (create-time (local-time:parse-timestring "2021-10-23T12:34:56Z"))
           (update-time (local-time:parse-timestring "2021-11-23T12:34:56Z")))
      (with-random-database-file (:connectp nil)
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
            (mito:insert-dao
             (make-instance 'legacy-concept
                            :uuid (car pair)
                            :name (cdr pair)
                            :content-format ""
                            :content ""
                            :created-at create-time
                            :updated-at update-time)))
          (dolist (pair concept-relation-daos)
            (mito:insert-dao (make-instance 'legacy-concept-relation
                                            :source (car pair)
                                            :target (cdr pair))))
        
          ;; Run.
          (migration:run-migrations))
        
        (store:with-current-database
          (let* ((concepts (mito:select-dao 'store:concept))
                 (links (mito:select-dao 'store:concept-link))
                 (contains (find-if (op (string= "Contains" (store:name _)))
                                    concepts))
                 (contains-uuid (store:object-id contains))
                 (relates (find-if (op (string= "Relates" (store:name _)))
                                   concepts))
                 (relates-uuid (store:object-id relates)))
            ;; New concepts equal to the original ones.
            (is (equal concept-daos
                       (subseq (mapcar (lambda (concept)
                                         (cons (store:object-id concept)
                                               (store:name concept)))
                                       concepts)
                               0 5)))
            ;; 2 special concepts are inserted.
            (is (not (null contains)))
            (is (not (null relates)))
            ;; Links are inserted.
            (is (= 5 (length links)))
            (is (find-if (op (check-concept-link _ "0" contains-uuid "11")) links))
            (is (find-if (op (check-concept-link _ "0" contains-uuid "12")) links))
            (is (find-if (op (check-concept-link _ "11" contains-uuid "21")) links))
            (is (find-if (op (check-concept-link _ "11" contains-uuid "22")) links))
            (is (find-if (op (check-concept-link _ "12" relates-uuid "22")) links)))

          ;; Check timestamp.
          (let ((concept (mito:find-dao 'store:concept :id "0")))
            (is (local-time:timestamp= create-time
                                       (store:object-created-at concept)))))))))
