(in-package silver-brain)

(defvar *config-package* :silver-brain
  "The root package.")

(defvar *profile-env* "SB_PROFILE")

(defvar *profile* nil)

(setf (envy:config-env-var *config-package*) *profile-env*)

(envy:defconfig product
    `(:app-root "~/.silver-brain/"
      :debug nil
      :server (:port 5000
               :access-log nil)
      :database (:driver-name :sqlite3
                 :database-name "data/silver-brain.sqlite")))

(envy:defconfig develop
    `(:app-root "~/.silver-brain/"
      :debug t
      :server (:port 15000
               :access-log t)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-dev.sqlite")))

(envy:defconfig testing
    `(:app-root "~/.silver-brain/"
      :debug t
      :server (:port ,(find-port:find-port)
               :access-log nil)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-test.sqlite")))

(defun profiles ()
  "Return a list of valid profiles."
  '(:develop :product :testing))

(defun set-profile (profile)
  (check-type profile (member :develop :product :testing))
  (setf (uiop:getenv *profile-env*)
        (format nil "~a" (string-upcase profile)))
  (setf *profile* profile)
  (ensure-directories-exist (get-config :app-root))
  (uiop:chdir (get-config :app-root))
  (setf *default-pathname-defaults* (uiop:getcwd)))

(defun get-profile ()
  *profile*)

(defun get-config (&rest keys)
  "Return corresponding config.
Argument `keys` should be keywords that will be used as key to `getf`."
  (iter
    (with config = (envy:config *config-package*))
    (for key in keys)
    (setf config (getf config key))
    (finally (return config))))
