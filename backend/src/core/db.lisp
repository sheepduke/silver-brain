(in-package silver-brain)

(defun setup-db ()
  "Connect to and setup database."
  (mito:connect-toplevel (conf:database-driver-name)
                         :database-name (conf:database-file-name))
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))
