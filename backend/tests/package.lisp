(defpackage silver-brain/tests.core
  (:use #:cl
        #:silver-brain.core
        #:rove))

(defpackage silver-brain/tests.server
  (:use #:cl
        #:rove
        #:alexandria
        #:iterate
        #:trivia
        #:silver-brain.core
        #:silver-brain.server)
  (:import-from #:cl-json
                #:encode-json-to-string
                #:decode-json-from-string))
