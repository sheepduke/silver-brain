(in-package silver-brain-tests.service)

(setup
  (config:set-profile :test)
  (db:setup))

(teardown
  (db::delete-file))

(defun setup-test ()
  "Setup for each test."
  )
