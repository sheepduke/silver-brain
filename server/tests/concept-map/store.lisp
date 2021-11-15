(defpackage silver-brain-tests.concept-map.store
  (:use #:cl
        #:silver-brain-tests
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.store)
                    (#:concept-map.store #:silver-brain.concept-map.store)
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

(in-package silver-brain-tests.concept-map.store)

(def-suite* silver-brain.concept-map.store :in silver-brain-tests:silver-brain)

(defparameter *database-name* (make-random-database-name))

(defparameter *concepts*
  (list (make-instance 'store:concept
                       :uuid "1"
                       :name "Software")
        (make-instance 'store:concept
                       :uuid "2"
                       :name "Middleware")
        (make-instance 'store:concept
                       :uuid "3"
                       :name "Microsoft")
        (make-instance 'store:concept
                       :uuid "4"
                       :name "Relates")))

(defun setup ()
  (setf (silver-brain.config:active-profile) :test)
  (store:with-current-database
    (mito:ensure-table-exists 'store:concept)
    (mito:ensure-table-exists 'store:concept-link)
    (mapcar (op (mito:insert-dao _)) *concepts*)
    (mito:insert-dao (make-instance 'store:concept-link
                                    :source "1"
                                    :relation "4"
                                    :target "2"))))

(test create-database
  (setf (silver-brain.config:active-profile) :test)
  (let ((database-name (make-random-database-name)))
    (concept-map.store:create-database database-name)
    (store:with-database (database-name)
      (is (= 2 (length (mito:select-dao 'store:concept)))))
    (delete-database-file database-name)))

(test get-concept-by-uuid
  (let ((time1 (local-time:now)))
    (with-random-database-file
      (setup)
      (is (null (concept-map.store:get-concept-by-uuid "0")))
      (let* ((concept (concept-map.store:get-concept-by-uuid "1"))
             (time2 (local-time:now)))
        (is (string= "1" (uuid concept)))
        (is (string= "Software" (name concept)))
        (is (local-time:timestamp<= time1
                                    (created-at concept)
                                    time2))
        (is (local-time:timestamp<= time1
                                    (updated-at concept)
                                    time2))))))

(test search-concept-by-string
  (with-random-database-file
    (setup)
    (is (null (concept-map.store:search-concept-by-string '("wrong"))))
    (let ((concepts (concept-map.store:search-concept-by-string '("soft"))))
      (is (= 2 (length concepts)))
      (is (find-if (op (and (string= "1" (uuid _1))
                            (string= "Software" (name _1))))
                   concepts))
      (is (find-if (op (and (string= "3" (uuid _1))
                            (string= "Microsoft" (name _1))))
                   concepts)))
    (let ((concepts (concept-map.store:search-concept-by-string '("soft" "ware"))))
      (is (= 3 (length concepts)))
      (is (find-if (op (and (string= "1" (uuid _1))
                            (string= "Software" (name _1))))
                   concepts))
      (is (find-if (op (and (string= "2" (uuid _1))
                            (string= "Middleware" (name _1))))
                   concepts))
      (is (find-if (op (and (string= "3" (uuid _1))
                            (string= "Microsoft" (name _1))))
                   concepts)))))

(test create-concept
  (with-random-database-file
    (setup)
    (let ((uuid (concept-map.store:create-concept
                 :name "New"
                 :content "Content"
                 :content-type "text/plain")))
      (store:with-current-database
        (is (= 5 (mito:count-dao 'store:concept)))
        (is (mito:find-dao 'store:concept :uuid uuid))))))

(test update-concept
  (with-random-database-file
    (setup)
    (with-mocks ()
      (answer (cache:invalidate-if-exists "1"))
      (concept-map.store:update-concept "1" :name "New")
      
      (store:with-current-database
        (is (= 4 (mito:count-dao 'store:concept)))
        (is (string= "New" (store:name (mito:find-dao 'store:concept :uuid "1")))))

      (is (= 1 (length (invocations 'cache:invalidate-if-exists)))) 

      (signals error (concept-map.store:update-concept "5"
                                                       :name "New2")))))

(test used-as-link-p
  (with-random-database-file
    (setup)
    (is (concept-map.store:used-as-link-p "4"))
    (is (not (concept-map.store:used-as-link-p "1")))))

(test delete-concept
  (with-random-database-file
    (setup)
    (with-mocks ()
      (answer (cache:invalidate-if-exists "1"))
      (concept-map.store:delete-concept "1")
      (store:with-current-database
        (is (= 3 (mito:count-dao 'store:concept)))
        (is (= 0 (mito:count-dao 'store:concept-link))))
      (is (= 1 (length (invocations 'cache:invalidate-if-exists))))))

  (with-random-database-file
    (setup)
    (with-mocks ()
      (answer (cache:invalidate-if-exists "4"))
      (concept-map.store:delete-concept "4")
      (store:with-current-database
        (is (= 3 (mito:count-dao 'store:concept)))
        (is (= 0 (mito:count-dao 'store:concept-link))))
      (is (= 1 (length (invocations 'cache:invalidate-if-exists)))))))

(test get-links
  (with-random-database-file
    (setup)
    (labels ((check-link (link)
               (is (string= "1" (uuid (source link))))
               (is (string= "Software" (name (source link))))
               (is (string= "4" (uuid (relation link))))
               (is (string= "Relates" (name (relation link))))
               (is (string= "2" (uuid (target link))))
               (is (string= "Middleware" (name (target link))))))

      (with-mocks ()
        (answer (cache:get-concept-name "1") "Software")
        (answer (cache:get-concept-name "2") "Middleware")
        (answer (cache:get-concept-name "4") "Relates")
        (check-link (car (concept-map.store:get-links :source "1")))
        (check-link (car (concept-map.store:get-links :target "2")))
        (check-link (car (concept-map.store:get-links :source "1" :target "2")))
        (check-link (car (concept-map.store:get-links :source "1"
                                                      :relation "4"
                                                      :target "2")))
        (is (null (concept-map.store:get-links :source "3")))))))

(test create-link
  (with-random-database-file
    (setup)
    (concept-map.store:create-link "1" "4" "2")
    (store:with-current-database
      (is (= 1 (mito:count-dao 'store:concept-link))))
    (concept-map.store:create-link "1" "4" "3")
    (store:with-current-database
      (is (= 2 (mito:count-dao 'store:concept-link))))))

(test delete-link
  (with-random-database-file
    (setup)
    (store:with-current-database
      (mito:insert-dao (make-instance 'store:concept-link
                                      :source "1"
                                      :relation "4"
                                      :target "3"))
      (mito:insert-dao (make-instance 'store:concept-link
                                      :source "3"
                                      :relation "4"
                                      :target "1"))
      (is (= 3 (mito:count-dao 'store:concept-link))))
    (concept-map.store:delete-links :source "1")
    (store:with-current-database
      (is (= 1 (mito:count-dao 'store:concept-link))))
    (concept-map.store:delete-links :source "3" :target "1")
    (store:with-current-database
      (is (= 0 (mito:count-dao 'store:concept-link))))))

;; (setf 5am:*run-test-when-defined* t)
