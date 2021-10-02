(defpackage silver-brain.store
  (:nicknames store)
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:export #:concept
           #:concept-uuid
           #:concept-name
           #:concept-content-type
           #:concept-content
           #:concept-link
           #:concept-link-uuid
           #:concept-link-source
           #:concept-link-target
           #:object-created-at
           #:object-updated-at           
           #:start
           #:stop))

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

(defun start ()
  (unless (mito.connection:connected-p)
    (mito:connect-toplevel :sqlite3 :database-name (config:database-file))
    (store.migration:run-migrations)))

(defun stop ()
  (when (mito.connection:connected-p)
    (mito:disconnect-toplevel)))
