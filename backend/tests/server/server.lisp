(in-package silver-brain-tests.server)

(defun url (format-string &rest args)
  (apply #'format
         (append
          (list nil (concatenate 'string
                                 "http://localhost:"
                                 (format nil "~a/api" (config:server-port))
                                 format-string))
          args)))

(defvar *software* nil)
(defvar *emacs* nil)
(defvar *vim* nil)
(defvar *nano* nil)

(defun setup-test ()
  (service::purge)
  (setf *software* (service:create-concept "Software" "Software content." ""))
  (setf *emacs* (service:create-concept "Emacs" "Emacs content." ""))
  (setf *vim* (service:create-concept "Vim" "Vim content." ""))
  (setf *nano* (service:create-concept "Nano" "Nano content." ""))
  (service:make-child *software* *emacs*)
  (service:make-child *software* *vim*)
  (service:make-friend *emacs* *vim*))

(defun http-get (control-string &rest format-args)
  (json:decode-json-from-string
   (flex:octets-to-string
    (http-request (apply #'url control-string format-args) :keep-alive nil))))

(setup
  (config:set-profile :test)
  (db:setup)
  (server:start))

(teardown
  (server:stop)
  (db::delete-db))

(defhook :before
  (setup-test))


(deftest test-get-concepts
  (let ((result (http-get "/concepts")))
    (ok (= (length result) 4)
        "Returns 4 results.")
    (ok (member (concept-uuid *software*)
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
    ((_ code headers)
     (ok (= code 201)
         "Returns 201.")
     (ok (str:containsp "/concepts/" (assoc-value headers :location))
         "Location header contains UUID."))))


(deftest test-get-concept-by-id
  (let ((result (http-get "/concepts/~a" (concept-uuid *software*))))
    (ok (string= (assoc-value result :uuid)
                 (concept-uuid *software*))))
  (multiple-value-match (http-request (url "/concepts/1234"))
    ((_ code)
     (ok (= code 404)
         "Returns 404 when :id is wrong."))))


(deftest test-put-concept-by-id
  (testing "PUT /concepts/:id"
    (multiple-value-match (http-request (url "/concepts/1234") :method :put)
      ((_ code)
       (ok (= code 404)
           "Returns 404 when :id is wrong.")))
    (multiple-value-match (http-request (url "/concepts/~a"
                                             (concept-uuid *software*))
                                        :method :put
                                        :content (json:encode-json-to-string
                                                  '((:name . "SOFTWARE"))))
      ((_ code)
       (ok (= code 200)
           "Concept got modified.")))))


(deftest test-delete-concept-by-id
  (testing "DELETE /concepts/:id"
    (multiple-value-match (http-request (url "/concepts/~a"
                                             (concept-uuid *software*))
                                        :method :delete)
      ((_ code)
       (ok (= code 200)
           "DELETE succeeded.")))
    (multiple-value-match (http-request (url "/concepts/~a"
                                             (concept-uuid *software*)))
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
                          (concept-uuid *emacs*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (concept-uuid *software*))
        "Only Software is Emacs's parent.")))


(deftest test-add-parent
  (http-request (url "/concepts/~a/parents/~a"
                     (concept-uuid *emacs*)
                     (concept-uuid *nano*))
                :method :put)
  (ok (= (length (concept-parents *emacs*)) 2)
      "Emacs now has 2 parents.")
  
  (ok (concept-childp *nano* *emacs*)
      "Nano is now a parent of Emacs")

  (multiple-value-match (http-request (url "/concepts/~a/parents/~a"
                                           (concept-uuid *emacs*)
                                           (concept-uuid *emacs*))
                                      :method :put)
    ((_ code)
     (ok (= code 400)
         "One concept cannot be a parent of itself."))))


(deftest test-delete-parent
  (http-request (url "/concepts/~a/parents/~a"
                     (concept-uuid *emacs*)
                     (concept-uuid *software*))
                :method :delete)
  (ok (= (length (concept-parents *emacs*)) 0)
      "Emacs now has no parent."))


(deftest test-get-concept-children
  (let ((result (http-get "/concepts/~a/children"
                          (concept-uuid *software*))))
    (ok (= (length result) 2)
        "Returns 2 result.")
    (ok (not (member (concept-uuid *software*)
                     (mapcar (lambda (c) (assoc-value c :uuid)) result)
                     :test #'string=))
        "Emacs and Vim are Software's child.")))


(deftest test-add-child
  (http-request (url "/concepts/~a/children/~a"
                     (concept-uuid *software*)
                     (concept-uuid *nano*))
                :method :put)
  (ok (= (length (concept-parents *nano*)) 1)
      "Nano has 1 parent.")

  (multiple-value-match (http-request (url "/concepts/~a/children/~a"
                                           (concept-uuid *emacs*)
                                           (concept-uuid *emacs*))
                                      :method :put)
    ((_ code)
     (ok (= code 400)
         "One concept cannot be a child of itself."))))


(deftest test-delete-child
  (http-request (url "/concepts/~a/children/~a"
                     (concept-uuid *software*)
                     (concept-uuid *emacs*))
                :method :delete)
  (ok (= (length (concept-children *software*)) 1)
      "Software has 1 child now.")
  (ok (= (length (concept-parents *emacs*)) 0)
      "Emacs has no parent."))


(deftest test-get-concept-friends
  (let ((result (http-get "/concepts/~a/friends"
                          (concept-uuid *emacs*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (concept-uuid *vim*))
        "Vim is a friend of Emacs."))
  (let ((result (http-get "/concepts/~a/friends"
                          (concept-uuid *vim*))))
    (ok (= (length result) 1)
        "Returns 1 result.")
    (ok (string= (assoc-value (first result) :uuid)
                 (concept-uuid *emacs*))
        "Emasc is a friend of Vim.")))


(deftest test-add-concept-friend
  (http-request (url "/concepts/~a/friends/~a"
                     (concept-uuid *emacs*)
                     (concept-uuid *software*))
                :method :put)
  (ok (= (length (concept-friends *emacs*)) 2)
      "Emacs now has 2 friends.")
  (ok (= (length (concept-parents *emacs*)) 0)
      "Emacs now has no parent.")
  (ok (= (length (concept-friends *software*)) 1)
      "Software now has 1 friend.")
  (ok (= (length (concept-children *software*)) 1)
      "Software now has 1 child.")

  (multiple-value-match (http-request (url "/concepts/~a/friends/~a"
                                           (concept-uuid *emacs*)
                                           (concept-uuid *emacs*))
                                      :method :put)
    ((_ code)
     (ok (= code 400)
         "One concept cannot be a friend of itself."))))


(deftest test-delete-concept-friend
  (http-request (url "/concepts/~a/friends/~a"
                     (concept-uuid *emacs*)
                     (concept-uuid *vim*))
                :method :delete)
  (ok (= (length (concept-friends *emacs*)) 0)
      "Emacs has no friend now.")
  (ok (= (length (concept-friends *vim*)) 0)
      "Vim has no friend now."))
