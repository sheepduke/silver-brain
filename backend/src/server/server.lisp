(in-package silver-brain)

(defclass server (caveman2:<app>)
  ((handler :accessor handler
            :initform nil
            :documentation "Server that will be running."))
  (:documentation "The server."))

(defvar *server* (make-instance 'server)
  "The global web server.")

(defun start-server ()
  "Start the server."
  (unless (handler *server*)
    (setf (handler *server*)
          (clack:clackup (lack.builder:builder
                          (:static :path "/static/"
                                   :root "static/")
                          (if (get-config :server :access-log) :accesslog nil)
                          *server*)
                         :port (get-config :server :port)
                         :debug (get-config :debug)
                         :use-thread (not (equal (get-profile) :product))))))

(defun stop-server ()
  "Stop the server."
  (when (handler *server*)
    (clack:stop (handler *server*))
    (setf (handler *server*) nil)))