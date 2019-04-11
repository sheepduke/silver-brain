(in-package silver-brain/tests)

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

(defun setup-environment ()
  (setf rove:*enable-colors* t)
  (setf mito:*trace-sql-hooks* nil)
  (set-profile :testing)
  (setup-db))

(defun purge-db ()
  (uiop:delete-file-if-exists (get-config :database :database-name)))

(defun setup-test ()
  (brain::delete-all-concepts)
  (setf *software* (brain::add-concept "Software" "" ""))
  (setf *emacs* (brain::add-concept "Emacs" "" ""))
  (brain::become-child *software* *emacs*))

(setup
  (setup-environment)
  (start-server)
  (format t "Waiting 0.5 second for server to start...~&")
  (sleep 0.5))

(teardown
  (stop-server)
  (purge-db))

(defhook :before
  (setup-test))

(deftest test-get-concepts
  (let ((result (json:decode-json-from-string
                 (dex:get (url "/concepts") :keep-alive nil))))
    (ok (= (length result) 2)
        "Returns 2 results.")
    (ok (member (brain::concept-uuid *software*)
                (mapcar (lambda (alist) (assoc-value alist :uuid)) result)
                :test #'string=)
        "Contains correct concept.")))

(deftest test-post-concepts
  (match (multiple-value-list
          (dex:post (url "/concepts")
                    :content (json:encode-json-to-string
                              '((:name . "Vim")
                                (:content . "Content Vim")
                                (:content-format . "plain")))
                    :keep-alive nil))
    ((list _ code headers _ _)
     (ok (= code 201)
         "Returns 201.")
     (ok (gethash "location" headers)
         "Location header is set."))))

(deftest test-get-concept-by-id
  (testing "GET /concepts/:id"
    (let ((result (json:decode-json-from-string
                   (dex:get (url "/concepts/~a"
                                 (brain::concept-uuid *software*))))))
      (ok (string= (assoc-value result :uuid)
                   (brain::concept-uuid *software*))))
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
            (dex:put (url "/concepts/~a" (brain::concept-uuid *software*)))
            'dex:http-request-bad-request)
        "Returns 400 when no content is given.")))

(deftest test-delete-concept-by-id
  (testing "DELETE /concepts/:id"
    (ok (dex:delete (url "/concepts/~a" (brain::concept-uuid *software*)))
        "DELETE succeeded.")
    (ok (signals
         (dex:get (url "/concepts/~a" (brain::concept-uuid *software*)))
         'dex:http-request-not-found)
        "Delete UUID does not exist anymore."))
  (testing "Delete wrong concept"
    (ok (signals
            (dex:delete (url "/concepts/1234"))
            'dex:http-request-not-found)
        "Returns 404 when :id is invalid.")))

(deftest test-get-concept-parents
  (let ((result (json:decode-json-from-string
                 (dex:get (url "/concepts/~a/parents"
                               (brain::concept-uuid *emacs*))))))
    (ok (= (length result) 1)
        "Returns 1 result")))

;; (set-profile :develop)
;; (progn
;;   (purge-db)
;;   (setup-db)
;;   (setup-test))
;; (start-server)
