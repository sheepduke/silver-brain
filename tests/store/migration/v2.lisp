(unlisp:defpackage #:silver-brain-tests.store.migration.v2
  (:use #:unlisp
        #:lisp-unit2
        #:silver-brain-tests.common.util)
  (:local-nicknames (#:migration #:silver-brain.store.migration)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:v2 #:silver-brain.store.schema.v2)))

(in-package #:silver-brain-tests.store.migration.v2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(define-test run-with-empty-database (:tags :silver-brain.store
                                      :contexts #'temp-db-context)
  (migration:migrate :upto v2:schema-version)
  (assert-equal 0 (mito:count-dao 'v2:concept))
  (assert-equal 0 (mito:count-dao 'v2:concept-pair))
  (assert-equal 0 (mito:count-dao 'v2:concept-link))
  (assert-equal 0 (mito:count-dao 'v2:concept-alias))
  (assert-equal 0 (mito:count-dao 'v2:concept-attachment))
  (assert-equal v2:schema-version (migration:fetch-data-version)))

(define-test run-with-v1-data (:tags :silver-brain.store
                               :contexts '(temp-db-context
                                           data.v1:context))
  (migration:migrate :upto v2:schema-version)

  (let ((concepts (mito:select-dao 'v2:concept))
        (pairs (mito:select-dao 'v2:concept-pair))
        (links (mito:select-dao 'v2:concept-link))
        (aliases (mito:select-dao 'v2:concept-alias))
        (attachments (mito:select-dao 'v2:concept-attachment)))
    ;; Check data version.
    (assert-equal v2:schema-version (migration:fetch-data-version))

    ;; Check count of items in each table.
    (assert-equal 6 (list:count concepts))
    (assert-equal 2 (list:count pairs))
    (assert-equal 2 (list:count links))
    (assert-equal 0 (list:count aliases))
    (assert-equal 2 (list:count attachments))

    ;; Check concepts.
    (assert-true (lset:subset? data.v1:mock-concepts
                               concepts
                               :test (fun (legacy new)
                                       (and (string:= (v2:uuid new)
                                                      (v1:uuid legacy))))))

    (assert-true (lset:subset? '("Is parent of" "Is child of" "Relates to")
                               concepts
                               :test (fun (name concept)
                                       (and (string:= name
                                                      (v2:name concept))))))
    (let* ((parent (list:find-if concepts
                                 (op (string:= (v2:name _)
                                               "Is parent of"))))
           (parent-uuid (v2:uuid parent))
           (child (list:find-if concepts
                                (op (string:= (v2:name _)
                                              "Is child of"))))
           (child-uuid (v2:uuid child))
           (friend (list:find-if concepts
                                 (op (string:= (v2:name _)
                                               "Relates to"))))
           (friend-uuid (v2:uuid friend)))

      ;; Check pairs.
      (let ((expected (list:sort! (list (cons parent-uuid child-uuid)
                                        (cons friend-uuid friend-uuid))))
            (actual (list:sort! (list:map pairs
                                          (fun (pair) (cons (v2:concept-uuid pair)
                                                            (v2:other-uuid pair)))))))
        (assert-true (equal? expected actual)))
      
      ;; Check links.
      (let ((expected (list (list "0x01" parent-uuid "0x02")
                            (list "0x01" friend-uuid "0x03")))
            (actual (list:map links
                              (fun (link)
                                (list (v2:source-uuid link)
                                      (v2:relation-uuid link)
                                      (v2:target-uuid link))))))
        (assert-true (equal? expected actual)))

      ;; Check attachments.
      (let ((expected (pipe data.v1:mock-concepts
                            (list:remove-if (op (string:empty? (v1:content _))))
                            (list:map (fun (concept)
                                        (list (v1:uuid concept)
                                              (v1:content-format concept)
                                              (v1:content concept))))))
            (actual (list:map attachments
                              (fun (attachment)
                                (list (v2:concept-uuid attachment)
                                      (v2:content-type attachment)
                                      (v2:content attachment))))))
        (assert-true (equal? expected actual))))))

