(defpackage silver-brain
  (:use #:cl)
  (:export #:main))

(defpackage silver-brain.concept-map
  (:use #:cl))

(defpackage silver-brain.store
  (:use #:cl))

(defpackage silver-brain.store.migration
  (:use #:cl)
  (:import-from #:mitogrator))
