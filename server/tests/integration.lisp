(defpackage silver-brain-tests.integration
  (:use #:cl
        #:silver-brain-tests)
  (:local-nicknames (#:store #:silver-brain.store))
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

(defmacro with-test-context (&body body)
  `(let ((silver-brain.config:*profile* :test))
     (store:with-database (*database-name* :auto-create t :auto-migrate t))
     (silver-brain:stop)
     (wait-for-server t)
     (silver-brain:start)
     (wait-for-server nil)
     (unwind-protect (progn ,@body)
       (silver-brain:stop)
       (delete-database-file *database-name*)
       (wait-for-server t))))

(test integration
  (with-test-context
    (let (uuid-new uuid-software uuid-middleware uuid-includes)

      ;; Database.
      (is (str:emptyp (post "database" (jsown:new-js ("name" *database-name*)))))

      ;; Get concept.
      (signals dex:http-request-bad-request
        (get "concept/invalid-uuid"))

      (signals dex:http-request-not-found
        (get (format nil "concept/~a" (uuid:make-v4-uuid))))

      ;; Create concept.
      (setf uuid-new (post "concept" (jsown:new-js ("name" "Concept Name"))))
      (is (not (str:emptyp uuid-new)))

      (let ((json (jsown:parse (get (format nil "concept/~a" uuid-new)))))
        (is (string= "Concept Name" (jsown:val json "name"))))

      ;; Update concept.
      (patch (format nil "concept/~a" uuid-new)
             (jsown:new-js ("name" "New Name")))
      (let ((json (jsown:parse (get (format nil "concept/~a" uuid-new)))))
        (is (string= "New Name" (jsown:val json "name"))))

      ;; Insert concepts and links.
      (setf uuid-software
            (post "concept" (jsown:new-js ("name" "Software"))))
      (setf uuid-middleware
            (post "concept" (jsown:new-js ("name" "Middleware"))))
      (setf uuid-includes
            (post "concept" (jsown:new-js ("name" "Includes"))))
      (post "concept-link" (list (jsown:new-js
                                   ("source" uuid-software)
                                   ("relation" uuid-includes)
                                   ("target" uuid-middleware)
                                   ("directional" t))))

      ;; Get links.
      (let ((json (first (jsown:parse
                          (get (format nil "concept-link?source=~a&target=~a"
                                       uuid-software
                                       uuid-middleware))))))
        (is (string= uuid-software
                     (~>> (jsown:val-safe json "source")
                          (jsown:val-safe _ "uuid"))))
        (is (string= "Software"
                     (~>> (jsown:val-safe json "source")
                          (jsown:val-safe _ "name"))))
        (is (string= uuid-includes
                     (~>> (jsown:val-safe json "relation")
                          (jsown:val-safe _ "uuid"))))
        (is (string= "Includes"
                     (~>> (jsown:val-safe json "relation")
                          (jsown:val-safe _ "name"))))
        (is (string= uuid-middleware
                     (~>> (jsown:val-safe json "target")
                          (jsown:val-safe _ "uuid"))))
        (is (string= "Middleware"
                     (~>> (jsown:val-safe json "target")
                          (jsown:val-safe _ "name")))))

      (let ((json (jsown:parse (get "concept-link?source=123"))))
        (is (null json)))

      ;; Delete concept and links.
      (let* ((uuid (post "concept"
                         (jsown:new-js ("name" "Wrong one"))))
             (json (jsown:parse (get (format nil "concept/~a" uuid)))))
        (is (string= "Wrong one" (jsown:val json "name")))

        ;; Create links.
        (post "concept-link" (list (jsown:new-js
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
        (let* ((url (format nil "concept-link?source=~a" uuid))
               (links (jsown:parse (get url))))
          (is (= 2 (length links))))
        (let* ((url (format nil "concept-link?target=~a" uuid))
               (links (jsown:parse (get url))))
          (is (= 1 (length links))))

        ;; Delete links.
        (delete (format nil "concept-link?source=~a" uuid))
        (let* ((url (format nil "concept-link?source=~a" uuid))
               (links (jsown:parse (get url))))
          (is (null links)))
        (let* ((url (format nil "concept-link?target=~a" uuid))
               (links (jsown:parse (get url))))
          (is (= 1 (length links))))

        ;; Delete concept.
        (delete (format nil "concept/~a" uuid))
        (signals dex:http-request-not-found (get (format nil "concept/~a" uuid)))
        (let* ((url (format nil "concept-link?target=~a" uuid))
               (links (jsown:parse (get url))))
          (is (null links)))))))

;; (setf 5am:*run-test-when-defined* t)
