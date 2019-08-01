(in-package silver-brain/tests)

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:"
                                 (format nil "~a" (conf:server-port))
                                 format-string))
          args)))

(defvar *software* nil)
(defvar *emacs* nil)
(defvar *vim* nil)
(defvar *nano* nil)

(defun purge-db ()
  (uiop:delete-file-if-exists (conf:database-file-name)))

(defun setup-test ()
  (brain::delete-all-concepts)
  (setf *software* (brain::add-concept "Software" "Software content." ""))
  (setf *emacs* (brain::add-concept "Emacs" "Emacs content." ""))
  (setf *vim* (brain::add-concept "Vim" "Vim content." ""))
  (setf *nano* (brain::add-concept "Nano" "Nano content." ""))
  (brain::become-child *software* *emacs*)
  (brain::become-child *software* *vim*)
  (brain::become-friend *emacs* *vim*))

(defun http-get (control-string &rest format-args)
  (json:decode-json-from-string
   (http-request (apply #'url control-string format-args) :keep-alive nil)))

(defun uuid (concept)
  (brain::concept-uuid concept))

(setup
  (conf:set-profile :test)
  (setup-db)
  (start-server))

(teardown
  (stop-server)
  (purge-db))

(defhook :before
  (setup-test))

(deftest test-get-concepts
  (let ((result (http-get "/concepts")))
    (ok (= (length result) 4)
        "Returns 4 results.")
    (ok (member (uuid *software*)
                (mapcar (lambda (alist) (assoc-value alist :uuid)) result)
                :test #'string=)
        "Contains correct concept.")))



(deftest test-post-concepts
  (multiple-value-match (http-request (url "/concepts")
                                      :method :post
                                      :content (json:encode-json-to-string
                                                '((:name . "Vim")
                                                  (:content . "Content Vim")
                                                  (:content-format . "plain"))))
    ((_ code headers _ _ _ _)
     (ok (= code 201)
         "Returns 201.")
     (ok (str:containsp "/concepts/" (assoc-value headers :location))
         "Location header contains UUID."))))

(deftest test-get-concept-by-id
  (let ((result (http-get "/concepts/~a" (uuid *software*))))
    (ok (string= (assoc-value result :uuid)
                 (uuid *software*))))
  (multiple-value-match (http-request (url "/concepts/1234"))
    ((_ code _ _ _ _ _)
     (ok (= code 404)
         "Returns 404 when :id is wrong."))))

(deftest test-put-concept-by-id
  (testing "PUT /concepts/:id"
    (multiple-value-match (http-request (url "/concepts/1234") :method :put)
      ((_ code _ _ _ _ _)
       (ok (= code 404)
           "Returns 404 when :id is wrong.")))
    (multiple-value-match (http-request (url "/concepts/~a" (uuid *software*))
                                        :method :put)
      ((_ code _ _ _ _ _)
       (ok (= code 400)
           "Returns 400 when no content is given.")))))

(deftest test-delete-concept-by-id
  (testing "DELETE /concepts/:id"
    (multiple-value-match (http-request (url "/concepts/~a" (uuid *software*))
                                        :method :delete)
      ((_ code)
       (ok (= code 200)
           "DELETE succeeded.")))
    (multiple-value-match (http-request (url "/concepts/~a" (uuid *software*)))
      ((_ code)
       (ok (= code 404)
           "Delete UUID does not exist anymore."))))
  (testing "Delete wrong concept"
    (multiple-value-match (http-request (url "/concepts/1234")
                                        :method :delete)
      ((_ code)
       (ok (= code 404)
           "Returns 404 when :id is invalid.")))))

(deftest test-get-concept-parents
  (let ((result (http-get "/concepts/~a/parents"
                          (uuid *emacs*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (uuid *software*))
        "Only Software is Emacs's parent.")))

(deftest test-add-parent
  (http-request (url "/concepts/~a/parents/~a"
                     (uuid *emacs*)
                     (uuid *nano*))
                :method :put)
  (ok (= (length (brain::get-concept-parents *emacs*)) 2)
      "Emacs now has 2 parents.")
  
  (ok (brain::linkedp *nano* *emacs*)
      "Nano is now a parent of Emacs"))

(deftest test-delete-parent
  (http-request (url "/concepts/~a/parents/~a"
                     (uuid *emacs*)
                     (uuid *software*))
                :method :delete)
  (ok (= (length (brain::get-concept-parents *emacs*)) 0)
      "Emacs now has no parent."))

(deftest test-get-concept-children
  (let ((result (http-get "/concepts/~a/children"
                          (uuid *software*))))
    (ok (= (length result) 2)
        "Returns 2 result.")
    (ok (not (member (uuid *software*)
                     (mapcar (lambda (c) (assoc-value c :uuid)) result)
                     :test #'string=))
        "Emacs and Vim are Software's child.")))

(deftest test-add-child
  (http-request (url "/concepts/~a/children/~a" (uuid *software*) (uuid *nano*))
                :method :put)
  (ok (= (length (brain::get-concept-parents *nano*)) 1)
      "Nano has 1 parent."))

(deftest test-delete-child
  (http-request (url "/concepts/~a/children/~a" (uuid *software*) (uuid *emacs*))
                :method :delete)
  (ok (= (length (brain::get-concept-children *software*)) 1)
      "Software has 1 child now.")
  (ok (= (length (brain::get-concept-parents *emacs*)) 0)
      "Emacs has no parent."))

(deftest test-get-concept-friends
  (let ((result (http-get "/concepts/~a/friends"
                          (uuid *emacs*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (uuid *vim*))
        "Vim is a friend of Emacs."))
  (let ((result (http-get "/concepts/~a/friends"
                          (uuid *vim*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (uuid *emacs*))
        "Emasc is a friend of Vim.")))

(deftest test-add-concept-friend
  (http-request (url "/concepts/~a/friends/~a" (uuid *emacs*) (uuid *software*))
                :method :put)
  (ok (= (length (brain::get-concept-friends *emacs*)) 2)
      "Emacs now has 2 friends.")
  (ok (= (length (brain::get-concept-parents *emacs*)) 0)
      "Emacs now has no parent.")
  (ok (= (length (brain::get-concept-friends *software*)) 1)
      "Software now has 1 friend.")
  (ok (= (length (brain::get-concept-children *software*)) 1)
      "Software now has 1 child."))

(deftest test-delete-concept-friend
  (http-request (url "/concepts/~a/friends/~a" (uuid *emacs*) (uuid *vim*))
                :method :delete)
  (ok (= (length (brain::get-concept-friends *emacs*)) 0)
      "Emacs has no friend now.")
  (ok (= (length (brain::get-concept-friends *vim*)) 0)
      "Vim has no friend now."))

;; (progn
;;   (conf:set-profile :dev)
;;   (purge-db)
;;   (setup-db)
;;   (setup-test)
;;   (stop-server)
;;   (start-server))

;; (asdf:test-system :silver-brain)
