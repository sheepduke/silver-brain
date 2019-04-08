(in-package silver-brain/tests.server)

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:"
                                 (format nil "~a" (get-config :server :port))
                                 format-string))
          args)))

(defvar *software* nil)
(defvar *emacs* nil)

(defun setup-test-server ()
  (setf mito:*trace-sql-hooks* nil)
  (core:set-profile :test)
  (purge-db)
  (apply 'core:setup-db
         (core:get-config :database))
  ;; (become-child *software* *emacs*)
  (setf *software* (add-concept "Software" "" ""))
  (setf *emacs* (add-concept "Emacs" "" "")))

(defun purge-db ()
  (uiop:delete-file-if-exists (get-config :database :database-name)))

(setup
  (setf rove:*enable-colors* t)
  (setup-test-server)
  (start-server)
  (format t "Waiting 0.5 second for server to start...~&")
  (sleep 0.5))

(teardown
  (stop-server)
  (purge-db))

(defhook :before
  (setup-test-server))

(deftest test-get-concepts
  (testing "GET /concepts/"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/") :keep-alive nil))))
      (ok (= (length result) 2)
          "Returns 2 results.")
      (ok (member (concept-uuid *software*)
                  (mapcar (lambda (alist) (assoc-value alist :uuid)) result)
                  :test #'string=)
          "Contains correct concept."))))

(deftest test-post-concepts
  (testing "POST /concepts/"
    (match (multiple-value-list
            (dex:post (url "/concepts/")
                          :content (encode-json-to-string
                                    '((:name . "Vim")
                                      (:content . "Content Vim")
                                      (:content-format . "plain")))
                          :keep-alive nil))
      ((list _ code headers _ _)
       (ok (= code 201)
           "Returns 201.")
       (ok (gethash "location" headers)
           "Location header is set.")))))

(deftest test-get-concept-by-id
  (testing "GET /concepts/:id"
    (let ((result (decode-json-from-string
                   (dex:get (url "/concepts/~a" (concept-uuid *software*))))))
      (ok (string= (assoc-value result :uuid) (concept-uuid *software*))))
    (ok (signals
            (dex:get (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")))

(deftest test-put-concept-by-id
  (testing "PUT /concepts/:id"
    (ok (signals
            (dex:put (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is wrong.")
    (ok (signals
            (dex:put (url "/concepts/~a" (concept-uuid *software*)))
            'dex:http-request-bad-request)
        "Returns 400 when no content is given.")))

(deftest test-delete-concept-id
  (testing "DELETE /concepts/:id"
    (ok (dex:delete (url "/concepts/~a" (concept-uuid *software*)))
        "DELETE succeeded."))
  (testing "Delete wrong concept"
    (ok (signals
            (dex:delete (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is invalid.")))
