(defpackage silver-brain/db
  (:nicknames db)
  (:use #:cl)
  (:import-from :silver-brain/config)
  (:import-from :silver-brain/db/concept)
  (:import-from :silver-brain/db/concept-relation)
  (:export #:setup #:disconnect))

(in-package silver-brain/db)

(defun setup ()
  "Connect to and setup database."
  (mito:connect-toplevel (config:database-driver-name)
                         :database-name (config:database-file-name))
  (mito:ensure-table-exists 'db/concept:concept)
  (mito:ensure-table-exists 'db/concept-relation-dao:concept-relation))

(defun disconnect ()
  "Disconnect from database."
  (mito:disconnect-toplevel))

(defun purge ()
  "Purge database from CONNECTION."
  (mito:delete-by-values 'db/concept:concept)
  (mito:delete-by-values 'db/concept-relation-dao:concept-relation))

(defun delete-db ()
  "Delete DB file."
  (uiop:delete-file-if-exists (config:database-file-name)))
