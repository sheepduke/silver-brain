(defpackage silver-brain-tests.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache))
  (:import-from #:fiveam
                #:is
                #:def-test
                #:def-suite*))

(in-package silver-brain-tests.concept-map.cache)

(def-suite* silver-brain.concept-map.cache :in silver-brain-tests::silver-brain)

(def-test get-concept-name ()
  ;; (let ((actor-system (asys:make-actor-system)))
  ;;   ())
  (is (= 2 2)))
