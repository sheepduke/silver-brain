(defpackage silver-brain-tests.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:is
                #:def-test
                #:def-suite*)
  (:import-from #:cl-mock
                #:answer
                #:with-mocks))

(in-package silver-brain-tests.concept-map.cache)

(def-suite* silver-brain.concept-map.cache :in silver-brain-tests:silver-brain)

(def-test get-concept-name ()
  (let ((concept-name "Alpha"))
    (with-mocks ()
      (answer (mito:find-dao 'store:concept :uuid "1")
        (make-instance 'store:concept :uuid "1" :name concept-name))
      (cache:start)
      (is (string= concept-name (cache:get-concept-name "1"))
          "First hit should return database value")
      (is (string= concept-name (cache:get-concept-name "1"))
          "Second hit should return cached value")
      (is (equal nil (cache:get-concept-name "2"))
          "Non-existing hit should return NIL")
      (is  (equal '((mito:find-dao store:concept :uuid "1")
                   (mito:find-dao store:concept :uuid "2"))
                  (cl-mock:invocations 'mito:find-dao))
          "Database should be invoked exactly twice with uuid (1, 2)")
      (cache:stop))))
