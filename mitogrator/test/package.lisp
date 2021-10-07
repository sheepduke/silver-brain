(defpackage mitogrator-test
  (:use #:cl)
  (:import-from #:fiveam
                #:signals
                #:test
                #:def-suite*
                #:is)
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:serapeum
                #:op)
  (:export #:mitogrator))

(in-package mitogrator-test)

(5am:def-suite mitogrator)
