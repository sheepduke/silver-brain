(defpackage silver-brain/tests/server
  (:use #:cl
        #:rove
        #:alexandria
        #:iterate)
  (:import-from #:cl-json
                #:encode-json-to-string
                #:decode-json-from-string))
(in-package silver-brain/tests/server)

(defvar *software* (make-instance 'concept:concept
                                  :name "Software"
                                  :content "Software Content"))

(defvar *emacs* (make-instance 'concept:concept
                               :name "Emacs"
                               :content "Emacs Content"))

(defvar *port* 15000
  "Port to use.")

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:"
                                 (format nil "~a" *port*)
                                 format-string))
          args)))

(defun setup-server ()
  (let* ((concept-map (make-instance 'concept-map:concept-map)))
    (concept:become-child *software* *emacs*)
    (concept-map:add-concept concept-map *software*)
    (concept-map:add-concept concept-map *emacs*)
    (server:setup concept-map)))

(setup
  (setup-server)
  (server:start :port *port* :debug t)
  (format t "Waiting 1 second for server to start...~&")
  (sleep 1))

(teardown
  (server:stop))

(deftest server
  
  (testing "GET /concepts/"
    (let ((result (decode-json-from-string
                   (dexador:get (url "/concepts/")))))
      (ok (= (length result) 2)
          "Returns 2 results.")
      (ok (member (concept:id *software*)
                  (mapcar (lambda (alist) (assoc-value alist :id)) result)
                  :test #'string=)
          "Contains correct concept.")))

  (testing "GET /concepts/:id"
    (let ((result (decode-json-from-string
                   (dexador:get (url "/concepts/~a" (concept:id *software*))))))
      (ok (string= (assoc-value result :id) (concept:id *software*)))))

  )
(setup-server)
;; (server:start)
(let ((*port* 5000))
  (multiple-value-bind (code response headers)
      (dexador:post "http://localhost:5000/concepts/"
                    :content (encode-json-to-string
                              '((:name . "Vim")
                                (:content . "Content Vim"))))
    (print code)
    (iter (for (key value) in-hashtable headers)
      (format t "~a => ~a~&" key value))))
