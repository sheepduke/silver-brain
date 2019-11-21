(defpackage silver-brain-tests.core
  (:use #:cl #:rove #:silver-brain.core))

(defpackage silver-brain-tests.server
  (:use #:cl #:alexandria #:trivia #:rove #:silver-brain.core)
  (:import-from #:drakma
                #:http-request))
