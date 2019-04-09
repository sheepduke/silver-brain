(in-package silver-brain.core)

(defvar *config-package* :silver-brain.core
  "The root package.")

(defvar *profile-env* "SB_PROFILE")

(setf (envy:config-env-var *config-package*) *profile-env*)

(envy:defconfig product
    `(:debug t))

(envy:defconfig develop
    `(:debug nil
      :server (:port 5000)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-dev.sqlite")))

(envy:defconfig test
    `(:debug t
      :server (:port 5000)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-test.sqlite")))

(defun set-profile (profile)
  (check-type profile (member :develop :product :test))
  (setf (uiop:getenv *profile-env*)
        (format nil "~a" (string-upcase profile))))

(defun get-config (&rest keys)
  "Return corresponding config.
Argument `keys` should be keywords that will be used as key to `getf`."
  (iter
    (with config = (envy:config *config-package*))
    (for key in keys)
    (setf config (getf config key))
    (finally (return config))))
