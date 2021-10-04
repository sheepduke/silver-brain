(defpackage silver-brain.store
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config))
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:export #:start
           #:stop
           #:concept
           #:uuid
           #:name
           #:content-type
           #:content
           #:object-created-at
           #:object-updated-at
           #:concept-link
           #:source
           #:target))

(in-package silver-brain.store)

(mito:deftable concept ()
  ((uuid :col-type :string
         :reader uuid
         :primary-key t)
   (name :col-type :string
         :reader name)
   (content-type :col-type :string :initform ""
                 :reader content-type)
   (content :col-type :string :initform ""
            :reader content))
  (:keys name))

(mito:deftable concept-link ()
  ((uuid :col-type :string
         :reader uuid)
   (source :col-type :string
           :reader source)
   (target :col-type :string
           :reader target))
  (:keys uuid source target))

(defun start ()
  (unless (mito.connection:connected-p)
    (mito:connect-toplevel :sqlite3 :database-name (config:database-file))
    (store.migration:run-migrations)))

(defun stop ()
  (when (mito.connection:connected-p)
    (mito:disconnect-toplevel)))
