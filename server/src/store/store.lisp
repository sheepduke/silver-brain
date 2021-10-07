(defpackage silver-brain.store
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:migration #:silver-brain.store.migration))
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:import-from #:serapeum
                #:->)
  (:import-from #:alexandria
                #:with-gensyms)
  (:shadow #:get)
  (:export #:start
           #:stop
           #:get
           ;; Dao
           #:concept
           #:uuid
           #:name
           #:content-type
           #:content
           #:object-created-at
           #:object-updated-at
           #:concept-link
           #:source
           #:target
           #:with-database
           #:database-not-found-error
           #:database-name))

(in-package silver-brain.store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Database Accessors                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition database-not-found-error (error)
  ((database-name :type string :accessor database-name :initarg :database-name)))

(defmacro with-database ((database-name) &body body)
  (with-gensyms (g-database-name)
    `(let* ((,g-database-name ,database-name))
       (unless (uiop:file-exists-p ,g-database-name)
         (error 'database-not-found-error :database-name ,g-database-name))
       (dbi:with-connection (mito:*connection* :sqlite3
                                               :database-name ,g-database-name)
         (migration:run-migrations)
         ,@body))))

(defun get (class uuid)
  (mito:find-dao class :uuid uuid))

(-> select (symbol &optional sxql.clause:where-clause) standard-object)
(defun select (class &optional where)
  (mito:select-dao class where))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             DAO                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
