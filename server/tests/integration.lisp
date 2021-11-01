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
  (:shadow #:get))

(in-package silver-brain-tests.integration)

(def-suite* silver-brain.integration :in silver-brain)

(defparameter *server-port* (find-port:find-port))

(defparameter *database-name* (make-random-database-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Utility                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-url (uri)
  (format nil "http://localhost:~a/api/~a"
          (silver-brain.config:server-port)
          uri))

(defun get (uri)
  (dex:get (make-url uri)
           :headers `(("Database" . ,*database-name*))))

(defun post (uri &optional data)
  (dex:post (make-url uri)
            :content (if data (jsown:to-json data) nil)))

(defun setup ()
  (setf (silver-brain.config:active-profile) :test)
  (setf (silver-brain.config:server-port) (find-port:find-port))
  (store:with-database (*database-name* :auto-create t :auto-migrate t))
  (silver-brain:start)
  (wait-for-server nil))

(defun wait-for-server (stop-p)
  (loop for i from 1 to 10
        for can-stop-p = (handler-case (get "")
                           (usocket:connection-refused-error ()
                             (sleep 0.2)
                             stop-p))
        until can-stop-p
        finally (return (<= i 100))))

(defun teardown ()
  (silver-brain:stop)
  (uiop:delete-file-if-exists (merge-pathnames *database-name*
                                               (silver-brain.config:data-dir)))
  (wait-for-server t))

(test integration
  ;; Setup.
  (setup)

  ;; Database.
  (is (str:emptyp (post "database" (jsown:new-js ("name" *database-name*)))))

  ;; Concept.
  (signals dex:http-request-bad-request
    (get "concept/invalid-uuid"))

  (signals dex:http-request-not-found
    (get (format nil "concept/~a" (uuid:make-v4-uuid))))

  ;; Tear down.
  (teardown))
