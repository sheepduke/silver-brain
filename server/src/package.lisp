(defpackage silver-brain
  (:use #:cl)
  (:export #:main))

(defpackage silver-brain.store.migration
  (:use #:cl)
  (:import-from #:mitogrator))
