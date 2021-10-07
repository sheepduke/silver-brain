(defpackage silver-brain.store
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:connection #:silver-brain.store.connection))
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
;;;;                          Start/Stop                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (connection:start))

(defun stop ()
  (connection:stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Database Accessors                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition database-not-found-error (error)
  ((database-name :type string :accessor database-name :initarg :database-name)))

(defmacro with-database ((database-name) &body body)
  (with-gensyms (g-database-name)
    `(let* ((,g-database-name ,database-name)
            (mito:*connection* (connection:get ,g-database-name)))
       (if (null mito:*connection*)
           (error 'database-not-found-error :database-name ,g-database-name))
       ,@body)))

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
