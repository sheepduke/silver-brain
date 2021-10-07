(defpackage silver-brain-tests
  (:use #:cl)
  (:import-from #:fiveam
                #:def-suite*)
  (:export #:silver-brain))

(in-package silver-brain-tests)

(def-suite* silver-brain)
