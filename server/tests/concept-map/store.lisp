(defpackage silver-brain-tests.concept-map.store
  (:use #:cl
        #:silver-brain-tests
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.store)
                    (#:concept-map.store #:silver-brain.concept-map.store))
  (:import-from #:fiveam
                #:is
                #:test
                #:def-suite*)
  (:import-from #:cl-mock
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
    (mapcar (op (mito:insert-dao _)) *concepts*)))

(test create-database
  (setf (silver-brain.config:active-profile) :test)
  (let ((database-name (make-random-database-name)))
    (concept-map.store:create-database database-name)
    (store:with-database (database-name)
      (is (= 2 (length (mito:select-dao 'store:concept)))))
    (uiop:delete-file-if-exists (merge-pathnames database-name
                                                 (silver-brain.config:data-dir)))))

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
    (is (null (concept-map.store:search-concept-by-string "wrong")))
    (let ((concepts (concept-map.store:search-concept-by-string "soft")))
      (is (= 2 (length concepts)))
      (is (find-if (op (and (string= "1" (uuid _1))
                            (string= "Software" (name _1))))
                   concepts))
      (is (find-if (op (and (string= "3" (uuid _1))
                            (string= "Microsoft" (name _1))))
                   concepts)))))
