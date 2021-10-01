(in-package silver-brain.store)

(mito:deftable concept ()
  ((uuid :col-type :string
         :primary-key t)
   (name :col-type :string)
   (content-type :col-type :string :initform "")
   (content :col-type :string :initform ""))
  (:keys name)
  (:conc-name concept-))

(mito:deftable concept-link ()
  ((uuid :col-type :string)
   (source :col-type :string)
   (target :col-type :string))
  (:keys uuid source target)
  (:conc-name concept-link-))

(defun setup ()
  (unless (mito.connection:connected-p)
    (mito:connect-toplevel :sqlite3 :database-name (config:database-file))
    (store.migration:run-migrations)))
