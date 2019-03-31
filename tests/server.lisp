(defpackage silver-brain/tests/server
  (:use #:cl
        #:rove
        #:alexandria)
  (:import-from #:cl-json
                #:decode-json-from-string))
(in-package silver-brain/tests/server)

(defvar *software* (make-instance 'concept:concept
                                  :name "Software"
                                  :content "Software Content"))

(defvar *emacs* (make-instance 'concept:concept
                               :name "Emacs"
                               :content "Emacs Content"))

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:15000" format-string))
          args)))

(defun setup-server ()
  (let* ((concept-map (make-instance 'concept-map:concept-map)))
    (concept:become-child *software* *emacs*)
    (concept-map:add-concept concept-map *software*)
    (concept-map:add-concept concept-map *emacs*)
    (server:setup concept-map)))

(setup
  (setup-server)
  (server:start :port 15000 :debug t)
  (format t "Waiting 2 seconds for server to start...~&")
  (sleep 3))

(teardown
  (server:stop))

(deftest server
  
  (testing "API /concepts"
    (let ((result (decode-json-from-string
                   (dexador:get (url "/concepts")))))
      (ok (= (length result) 2)
          "Returns 2 results.")
      (ok (member (concept:id *software*)
                  (mapcar (lambda (alist) (assoc-value alist :id)) result)
                  :test #'string=)
          "Contains correct concept.")))

  (testing "API /concepts/:id"
    (let ((result (decode-json-from-string
                   (dexador:get (url "/concepts/~a" (concept:id *software*))))))
      (ok (string= (assoc-value result :id) (concept:id *software*))))))
