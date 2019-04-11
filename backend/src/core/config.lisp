(in-package silver-brain)

(defvar *config-package* :silver-brain
  "The root package.")

(defvar *profile-env* "SB_PROFILE")

(setf (envy:config-env-var *config-package*) *profile-env*)

(envy:defconfig product
    `(:debug nil
      :server (:port 5000)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain.sqlite")))

(envy:defconfig develop
    `(:debug t
      :server (:port 5000)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-dev.sqlite")))

(envy:defconfig testing
    `(:debug t
      :server (:port 5000)
      :database (:driver-name :sqlite3
                 :database-name "silver-brain-test.sqlite")))

(defun profiles ()
  "Return a list of valid profiles."
  '(:develop :product :testing))

(defun set-profile (profile)
  (check-type profile (member :develop :product :testing))
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
