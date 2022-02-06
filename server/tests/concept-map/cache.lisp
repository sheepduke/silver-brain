(defpackage silver-brain-tests.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:test
                #:is
                #:def-test
                #:def-suite*)
  (:import-from #:cl-mock
                #:answer
                #:with-mocks))

(in-package silver-brain-tests.concept-map.cache)

(def-suite* silver-brain.concept-map.cache :in silver-brain-tests:silver-brain)

(test get-concept-name
  (let ((concept-name "Alpha"))
    (with-mocks ()
      (answer (store:get 'store:concept "1")
        (make-instance 'store:concept :id "1" :name concept-name))
      (cache:start)
      (is (string= concept-name (cache:get-concept-name "1"))
          "First hit should return database value")
      (is (string= concept-name (cache:get-concept-name "1"))
          "Second hit should return cached value")
      (is (equal nil (cache:get-concept-name "2"))
          "Non-existing hit should return NIL")
      (is (equal '((store:get store:concept "1")
                   (store:get store:concept "2"))
                 (cl-mock:invocations 'store:get))
          "Database should be accessed exactly twice with uuid (1, 2)")
      (cache:stop))))
