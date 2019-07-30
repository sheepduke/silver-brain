(in-package silver-brain.config)

(defconfig
  (app-root #P"~/.silver-brain/" "The directory of Silver Brain application.")
  (debugp t "Whether to enable debug mode.")
  (server-port (find-port) "The port of back-end server.")
  (server-use-thread-p t "Whether to use multiple threads.
If not, the application hangs after starting the web server.")
  (server-access-log-p t "Whether to enable server access log.")
  (database-driver-name :sqlite3 "The driver name of database.")
  (database-file-name nil "The name of database file."))

(defprofile :test
  (server-access-log-p nil)
  (database-file-name "silver-brain-test.sqlite"))

(defprofile :dev
  (server-port 15000)
  (database-file-name "silver-brain-dev.sqlite"))

(defprofile :product
  (debugp nil)
  (server-port 5000)
  (server-access-log-p nil)
  (database-file-name "silver-brain.sqlite"))

(defun set-profile (profile)
  "Set the current profile to PROFILE."
  (setf (active-profile) profile)
  (ensure-directories-exist (app-root))
  (uiop:chdir (app-root))
  (setf *default-pathname-defaults* (app-root)))
