(defpackage silver-brain-tests.integration
  (:use #:cl
        #:silver-brain-tests)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:store #:silver-brain.store))
  (:import-from #:fiveam
                #:pass
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

(defun json-get (json &rest keys)
  (let ((value (jsown:val json (first keys))))
    (if (null (rest keys))
        value
        (apply #'json-get value (rest keys)))))

(defun json-get-uuid (json)
  (json-get json "uuid"))

(defun json-get-name (json)
  (json-get json "name"))

(defun get-concept (uuid)
  (jsown:parse (get (format nil "concepts/~a" uuid))))

(defun create-concept (name)
  (jsown:parse (post "concepts"
                     (jsown:new-js ("name" name)))))

(defun update-concept (uuid &key name)
  (let ((json (jsown:new-js)))
    (and name (jsown:extend-js json ("name" name)))
    (patch (format nil "concepts/~a" uuid) json)))

(defun delete-concept (uuid)
  (delete (format nil "concepts/~a" uuid)))

(defun create-link (source relation target directionalp)
  (post "concept-links" (jsown:new-js
                          ("source" source)
                          ("relation" relation)
                          ("target" target)
                          ("is-directional" directionalp))))

(defun delete-link (uuid)
  (delete (format nil "concept-links/~a" uuid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Tests                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test initialize
  (config:switch-profile :test)
  (silver-brain:stop)
  (wait-for-server t)
  (silver-brain:start)
  (wait-for-server nil)
  (store:with-database (*database-name* :auto-create t :auto-migrate t))
  (pass))

(test ping-server
  (is (str:emptyp (get ""))))

(test database-exists
  (is (str:emptyp (post "databases" (jsown:new-js ("name" *database-name*))))))

(test get-invalid-concept
  (signals dex:http-request-bad-request
    (get "concepts/invalid-uuid"))

  (signals dex:http-request-not-found
    (get (string-downcase (format nil "concepts/~a" (uuid:make-v4-uuid))))))

(test create-and-update-concept
  (let ((uuid-new (json-get (create-concept "Concept Name") "uuid")))

    (is (not (str:emptyp uuid-new)))

    (let ((concept (get-concept uuid-new)))
      (is (string= "Concept Name" (json-get concept "name"))))

    (let ((new-name "New Name"))
      (update-concept uuid-new :name new-name)
      (is (string= new-name
                   (json-get (get-concept uuid-new) "name"))))))

(test create-invalid-link
  (let* ((concept1 (create-concept "Test"))
         (uuid1 (json-get-uuid concept1))
         (concept2 (create-concept "Test 2"))
         (uuid2 (json-get-uuid concept2)))
    (delete-concept uuid2)

    (signals dex:http-request-bad-request
      (create-link uuid1 uuid1 uuid2 t))
    
    (delete-concept uuid1)))

(test insert-concepts-and-delete
  ;; Insert concepts and links.
  (let ((uuid-software (json-get-uuid (create-concept "Software")))
        (uuid-middleware (json-get-uuid (create-concept "Middleware")))
        (uuid-includes (json-get-uuid (create-concept "Includes"))))
    (create-link uuid-software uuid-includes uuid-middleware t)

    (let* ((concept (get-concept uuid-software))
           (link (first (json-get concept "links")))
           (source (json-get link "source"))
           (relation (json-get link "relation"))
           (target (json-get link "target")))
      (is (string= uuid-software (json-get-uuid source)))
      (is (string= "Software" (json-get-name source)))
      (is (string= uuid-includes (json-get-uuid relation)))
      (is (string= "Includes" (json-get-name relation)))
      (is (string= uuid-middleware (json-get-uuid target)))
      (is (string= "Middleware" (json-get-name target)))
      (is (equal t (json-get link "is-directional"))))

    ;; Delete concepts and links.
    (let* ((new-name "Wrong one")
           (concept (create-concept new-name))
           (uuid (json-get-uuid concept)))
      (create-link uuid uuid-includes uuid-software t)
      (create-link uuid-software uuid-includes uuid t)
      (create-link uuid uuid-includes uuid-middleware t)

      ;; Check link count.
      (let* ((concept (get-concept uuid))
             (links (json-get concept "links")))
        (is (= 3 (length links))))

      ;; Delete concept.
      (delete-concept uuid)
      (signals dex:http-request-not-found
        (get (format nil "concepts/~a" uuid))))

    ;; Delete everything.
    (delete-concept uuid-software)
    (delete-concept uuid-middleware)
    (delete-concept uuid-includes)))

(test cleanup
  (silver-brain:stop)
  (delete-database-file *database-name*)
  (wait-for-server t)
  (pass))
