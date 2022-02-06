(defpackage silver-brain-tests.cm-store
  (:use #:cl
        #:silver-brain-tests
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.store)
                    (#:config #:silver-brain.config)
                    (#:cm-store #:silver-brain.concept-map.store)
                    (#:cache #:silver-brain.concept-map.cache))
  (:import-from #:fiveam
                #:signals
                #:is
                #:test
                #:def-suite*)
  (:import-from #:cl-mock
                #:invocations
                #:answer
                #:with-mocks)
  (:import-from #:serapeum
                #:op))

(in-package silver-brain-tests.cm-store)

(def-suite* silver-brain.cm-store :in silver-brain-tests:silver-brain)

(defparameter *database-name* (make-random-database-name))

(defparameter *concepts*
  (list (make-instance 'store:concept
                       :id "1"
                       :name "Software")
        (make-instance 'store:concept
                       :id "2"
                       :name "Middleware")
        (make-instance 'store:concept
                       :id "3"
                       :name "Microsoft")
        (make-instance 'store:concept
                       :id "4"
                       :name "Relates")))

(defun setup ()
  (mito:ensure-table-exists 'store:concept)
  (mito:ensure-table-exists 'store:concept-link)
  (mapcar (op (mito:insert-dao _)) *concepts*)
  (mito:insert-dao (make-instance 'store:concept-link
                                  :id "142"
                                  :source "1"
                                  :relation "4"
                                  :target "2"
                                  :directionalp nil)))

(defmacro with-test-context (&body body)
  `(config:with-profile :test
     (with-random-database-file ()
       (setup)
       ,@body)))

(test create-database
  (config:with-profile :test
    (let ((database-name (make-random-database-name)))
      (cm-store:create-database database-name)
      (store:with-database (database-name)
        (is (= 2 (length (mito:select-dao 'store:concept)))))
      (delete-database-file database-name))))

(test get-concept-by-uuid
  (let ((time1 (local-time:now)))
    (with-test-context
      (with-mocks ()
        (answer (cache:get-concept-name "1") "S")
        (answer (cache:get-concept-name "2") "M")
        (answer (cache:get-concept-name "4") "R")
        
        (is (null (cm-store:get-concept-by-uuid "0")))
        (let* ((concept (cm-store:get-concept-by-uuid "1"))
               (time2 (local-time:now)))
          (is (string= "1" (uuid concept)))
          (is (string= "Software" (name concept)))
          (is (= 1 (length (links concept))))

          (let ((link (first (links concept))))
            (is (string= "142" (uuid link)))
            (is (string= "S" (name (source link))))
            (is (string= "M" (name (target link))))
            (is (string= "R" (name (relation link)))))

          (is (local-time:timestamp<= time1
                                      (create-time concept)
                                      time2))
          (is (local-time:timestamp<= time1
                                      (update-time concept)
                                      time2)))))))

(test search-concept-by-string
  (with-test-context
    (is (null (cm-store:search-concept-by-string '("wrong"))))
    (let ((concepts (cm-store:search-concept-by-string '("soft"))))
      (is (= 2 (length concepts)))
      (is (find-if (op (and (string= "1" (uuid _1))
                            (string= "Software" (name _1))))
                   concepts))
      (is (find-if (op (and (string= "3" (uuid _1))
                            (string= "Microsoft" (name _1))))
                   concepts)))
    (let ((concepts (cm-store:search-concept-by-string '("soft" "ware"))))
      (is (= 1 (length concepts)))
      (is (find-if (op (and (string= "1" (uuid _1))
                            (string= "Software" (name _1))))
                   concepts)))))

(test create-concept
  (with-test-context
    (let ((concept (cm-store:create-concept
                    :name "New"
                    :content "Content"
                    :content-type "text/plain")))
      (is (= 5 (mito:count-dao 'store:concept)))
      (is (mito:find-dao 'store:concept :id (uuid concept))))))

(test update-concept
  (with-test-context
    (with-mocks ()
      (answer (cache:update-if-exists "1" "New"))
      (cm-store:update-concept "1" :name "New")

      (is (= 4 (mito:count-dao 'store:concept)))
      (is (string= "New" (store:name (mito:find-dao 'store:concept :id "1"))))

      (is (= 1 (length (invocations 'cache:update-if-exists))))

      (signals error (cm-store:update-concept "5"
                                              :name "New2")))))

(test delete-concept
  (with-test-context
    (with-mocks ()
      (answer (cache:invalidate-if-exists "1"))
      (cm-store:delete-concept "1")
      (is (= 3 (mito:count-dao 'store:concept)))
      (is (= 0 (mito:count-dao 'store:concept-link)))
      (is (= 1 (length (invocations 'cache:invalidate-if-exists))))))

  (with-test-context
    (with-mocks ()
      (answer (cache:invalidate-if-exists "4"))
      (cm-store:delete-concept "4")

      (is (= 3 (mito:count-dao 'store:concept)))
      (is (= 0 (mito:count-dao 'store:concept-link)))
      (is (= 1 (length (invocations 'cache:invalidate-if-exists)))))))

(test create-link
  (with-test-context
    (with-mocks ()
      (answer (cache:get-concept-name "1") "A")
      (answer (cache:get-concept-name "2") "B")
      (answer (cache:get-concept-name "3") "C")
      (answer (cache:get-concept-name "4") "D")
      (cm-store:create-link "1" "4" "2" t)
      (is (= 2 (mito:count-dao 'store:concept-link)))
      (cm-store:create-link "3" "4" "1" nil)
      (is (= 3 (mito:count-dao 'store:concept-link)))
      (is (= 1 (mito:count-dao 'store:concept-link
                               :source "1"
                               :target "3"))))))

(test delete-link
  (with-test-context
    (mito:insert-dao (make-instance 'store:concept-link
                                    :id "143"
                                    :source "1"
                                    :relation "4"
                                    :target "3"
                                    :directionalp nil))
    (mito:insert-dao (make-instance 'store:concept-link
                                    :id "341"
                                    :source "3"
                                    :relation "4"
                                    :target "1"
                                    :directionalp nil))
    (is (= 3 (mito:count-dao 'store:concept-link)))
    (cm-store:delete-link "143")
    (cm-store:delete-link "341")
    
    (is (= 1 (mito:count-dao 'store:concept-link)))
    (cm-store:delete-link "142")
    (is (= 0 (mito:count-dao 'store:concept-link)))))
