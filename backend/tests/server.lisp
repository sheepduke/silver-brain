(defpackage silver-brain/tests/server
  (:use #:cl
        #:rove
        #:alexandria
        #:iterate
        #:trivia)
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
  (server:start :port *port* :debug t)
  (format t "Waiting 0.5 second for server to start...~&")
  (sleep 0.5))

(teardown
  (server:stop))

(defhook :before
  (setup-server))

(deftest get-concepts
  (testing "GET /concepts/"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/")))))
      (ok (= (length result) 2)
          "Returns 2 results.")
      (ok (member (concept:id *software*)
                  (mapcar (lambda (alist) (assoc-value alist :id)) result)
                  :test #'string=)
          "Contains correct concept."))))

(deftest post-concepts
  (testing "POST /concepts/"
    (match (multiple-value-list
            (dex:post (url "/concepts/")
                          :content (encode-json-to-string
                                    '((:name . "Vim")
                                      (:content . "Content Vim")))))
      ((list _ code headers _ _)
       (ok (= code 201)
           "Returns 201.")
       (ok (gethash "location" headers)
           "Location header is set.")))))

(deftest get-concept-id
  (testing "GET /concepts/:id"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/~a" (concept:id *software*))))))
      (ok (string= (assoc-value result :id) (concept:id *software*))))
    (ok (signals
            (dex:get (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")))

(deftest put-concept-id
  (testing "PUT /concepts/:id"
    (ok (signals
            (dex:put (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")
    (ok (signals
            (dex:put (url "/concepts/~a" (concept:id *software*)))
            'dex:http-request-bad-request)
        "Returns 400 when no content is given.")))

(deftest delete-concept-id
  (testing "DELETE /concepts/:id"
    (ok (dex:delete (url "/concepts/~a" (concept:id *software*)))
        "DELETE succeeded."))
  (testing "Delete wrong concept"
    (ok (signals
            (dex:delete (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is invalid.")))

;; (server:stop)
(server:start)

(let ((*port* 5000))
  ;; (server:setup (make-instance 'concept-map:concept-map))
  (setup-server)
  (run-test 'delete-concept-id))
