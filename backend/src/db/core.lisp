(in-package silver-brain.db)

(defun setup ()
  "Connect to and setup database."
  (mito:connect-toplevel (config:database-driver-name)
                         :database-name (config:database-file-name))
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))

(defun disconnect ()
  "Disconnect from database."
  (mito:disconnect-toplevel))

(defun purge ()
  "Purge database from CONNECTION."
  (mito:delete-by-values 'concept)
  (mito:delete-by-values 'concept-relation))

(defun delete-db ()
  "Delete DB file."
  (uiop:delete-file-if-exists (config:database-file-name)))
