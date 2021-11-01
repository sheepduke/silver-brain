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

(defun patch (uri data)
  (dex:patch (make-url uri)
             :content (jsown:to-json data)))

(defun post (uri data &key (with-database t))
  (dex:post (make-url uri)
            :content (if data (jsown:to-json data) nil)
            :headers (if with-database
                         `(("Database" . ,*database-name*))
                         nil)))

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

(defmacro with-teardown (&body body)
  `(unwind-protect (progn ,@body)
     (silver-brain:stop)
     (delete-database-file *database-name*)
     (wait-for-server t)))

(test integration
  ;; Setup.
  (setup)

  (with-teardown
    ;; Database.
    (is (str:emptyp (post "database" (jsown:new-js ("name" *database-name*))))) 

    ;; Create concept.
    (let ((uuid (post "concept" (jsown:new-js ("name" "Concept Name")))))
      (is (not (str:emptyp uuid)))

      (let ((json (jsown:parse (get (format nil "concept/~a" uuid)))))
        (is (string= "Concept Name" (jsown:val json "name")))))

    ;; Get concept.
    (signals dex:http-request-bad-request
      (get "concept/invalid-uuid"))

    (signals dex:http-request-not-found
      (get (format nil "concept/~a" (uuid:make-v4-uuid))))))

;; (setf (silver-brain.config:active-profile) :dev)
;; (silver-brain:start)
;; (silver-brain:stop)
