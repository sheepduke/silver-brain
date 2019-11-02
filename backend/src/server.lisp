(defpackage silver-brain/server
  (:nicknames server)
  (:use #:cl)
  (:import-from #:clack)
  (:import-from #:silver-brain/server/route)
  (:export #:start
           #:stop))

(in-package silver-brain/server)

(defclass server ()
  ((handler :accessor handler
            :initform nil
            :documentation "Server that will be running.")
   (app :accessor app
        :initform silver-brain/server/route:*app*
        :documentation "Ningle app."))
  (:documentation "The server."))

(defvar *server* (make-instance 'server)
  "The global web server.")

(defun start ()
  "Start the server."
  (unless (handler *server*)
    (setf (handler *server*)
          (clack:clackup (lack.builder:builder
                          (:static :path "/static/"
                                   :root "static/")
                          (if (config:server-access-log-p) :accesslog nil)
                          (app *server*))
                         :port (config:server-port)
                         :debug (config:debugp)
                         :use-thread (config:server-use-thread-p)
                         ))))

(defun stop ()
  "Stop the server."
  (when (handler *server*)
    (clack:stop (handler *server*))
    (setf (handler *server*) nil)))
