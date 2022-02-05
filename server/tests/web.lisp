(defpackage silver-brain-tests.integration
  (:use #:cl
        #:silver-brain-tests)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:signals
                #:is
                #:fail
                #:def-suite*
                #:test)
  (:import-from #:trivia
                #:match)
  (:import-from #:serapeum
                #:~>>)
  (:shadow #:get
           #:delete))

(in-package silver-brain-tests.integration)

(def-suite* silver-brain.integration :in silver-brain)

(defparameter *database-name* (make-random-database-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Utility                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-url (uri)
  (let ((url (format nil "http://localhost:~a/api/~a"
                     (silver-brain.config:server-port)
                     uri)))
    (log:trace "URL: ~a" url)
    url))

(defun get (uri)
  (dex:get (make-url uri)
           :headers `(("Database" . ,*database-name*))))

(defun patch (uri data)
  (dex:patch (make-url uri)
             :content (jsown:to-json data)
             :headers `(("Database" . ,*database-name*))))

(defun put (uri data)
  (dex:put (make-url uri)
             :content (jsown:to-json data)
             :headers `(("Database" . ,*database-name*))))

(defun post (uri data &key (with-database t))
  (dex:post (make-url uri)
            :content (if data (jsown:to-json data) nil)
            :headers (if with-database
                         `(("Database" . ,*database-name*))
                         nil)))
(defun delete (uri)
  (dex:delete (make-url uri)
              :headers `(("Database" . ,*database-name*))))

(defun wait-for-server (stop-p)
  (loop for i from 1 to 10
        for can-stop-p = (handler-case (get "")
                           (usocket:connection-refused-error ()
                             (sleep 0.2)
                             stop-p))
        until can-stop-p
        finally (return (<= i 100))))

(defun json-get (json &rest keys)
  (let ((value (jsown:val json (first keys))))
    (if (null (rest keys))
        value
        (apply #'json-get value (rest keys)))))

(defmacro with-test-context (&body body)
  `(let ((current-profile config::*profile*))
     (config:switch-profile :test)
     (store:with-database (*database-name* :auto-create t :auto-migrate t))
     (silver-brain:stop)
     (wait-for-server t)
     (silver-brain:start)
     (wait-for-server nil)
     (unwind-protect (progn ,@body)
       (silver-brain:stop)
       (delete-database-file *database-name*)
       (wait-for-server t)
       (and current-profile
            (config:switch-profile current-profile)))))

(test web
  (with-test-context
    (let (uuid-new uuid-software uuid-middleware uuid-includes)

      ;; Ping the server.
      (get "")

      ;; Database.
      (is (str:emptyp (post "databases" (jsown:new-js ("name" *database-name*)))))

      ;; Get concept.
      (signals dex:http-request-bad-request
        (get "concepts/invalid-uuid"))

      (signals dex:http-request-not-found
        (get (format nil "concepts/~a" (uuid:make-v4-uuid))))

      ;; Create concept.
      (setf uuid-new (post "concepts" (jsown:new-js ("name" "Concept Name"))))
      (is (not (str:emptyp uuid-new)))

      (let ((json (jsown:parse (get (format nil "concepts/~a" uuid-new)))))
        (is (string= "Concept Name" (jsown:val json "name"))))

      ;; Update concept.
      (patch (format nil "concepts/~a" uuid-new)
             (jsown:new-js ("name" "New Name")))
      (let ((json (jsown:parse (get (format nil "concepts/~a" uuid-new)))))
        (is (string= "New Name" (jsown:val json "name"))))

      ;; Insert concepts and links.
      (setf uuid-software
            (post "concepts" (jsown:new-js ("name" "Software"))))
      (setf uuid-middleware
            (post "concepts" (jsown:new-js ("name" "Middleware"))))
      (setf uuid-includes
            (post "concepts" (jsown:new-js ("name" "Includes"))))
      (post "concept-links" (list (jsown:new-js
                                    ("source" uuid-software)
                                    ("relation" uuid-includes)
                                    ("target" uuid-middleware)
                                    ("directional" t))))

      ;; Get concept and verify links.
      (let* ((json (jsown:parse (get (format nil "concepts/~a" uuid-software))))
             (link (first (jsown:val json "links"))))

        ;; Get links.
        (is (string= uuid-software
                     (json-get link "source" "uuid")))

        (is (string= "Software"
                     (json-get link "source" "name")))
        (is (string= uuid-includes
                     (json-get link "relation" "uuid")))
        (is (string= "Includes"
                     (json-get link "relation" "name")))
        (is (string= uuid-middleware
                     (json-get link "target" "uuid")))
        (is (string= "Middleware"
                     (json-get link "target" "name"))))

      ;; Delete concept and links.
      (let* ((uuid (post "concepts"
                         (jsown:new-js ("name" "Wrong one"))))
             (json (jsown:parse (get (format nil "concepts/~a" uuid)))))
        (is (string= "Wrong one" (jsown:val json "name")))

        ;; Create links.
        (post "concept-links" (list (jsown:new-js
                                      ("source" uuid)
                                      ("relation" uuid-includes)
                                      ("target" uuid-software)
                                      ("directional" t))
                                    (jsown:new-js
                                      ("source" uuid-software)
                                      ("relation" uuid-includes)
                                      ("target" uuid)
                                      ("directional" t))
                                    (jsown:new-js
                                      ("source" uuid)
                                      ("relation" uuid-includes)
                                      ("target" uuid-middleware)
                                      ("directional" t))))

        ;; Get links.
        (let* ((url (format nil "concepts/~a" uuid))
               (concept (jsown:parse (get url)))
               (links (json-get concept "links")))
          (is (= 3 (length links)))

          ;; Delete links.
          (delete (format nil "concept-links/~a"
                          (json-get (first links) "uuid")))

          (let* ((url (format nil "concepts/~a" uuid))
                 (concept (jsown:parse (get url))))
            (is (= 2 (length (json-get concept "links"))))))

        ;;   Delete concept.
        (delete (format nil "concepts/~a" uuid))
        (signals dex:http-request-not-found
          (get (format nil "concepts/~a" uuid)))))))
