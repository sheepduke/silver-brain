(defpackage silver-brain/db
  (:nicknames db)
  (:use #:cl)
  (:import-from :silver-brain/config))

(in-package silver-brain/db)

(defun setup-db ()
  "Connect to and setup database."
  (mito:connect-toplevel (config:database-driver-name)
                         :database-name (config:database-file-name))
  (mito:ensure-table-exists 'concept)
  (mito:ensure-table-exists 'concept-relation))
