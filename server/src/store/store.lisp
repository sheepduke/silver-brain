(defpackage silver-brain.store
  (:use #:cl)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:migration #:silver-brain.store.migration))
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:import-from #:serapeum
                #:op
                #:->)
  (:import-from #:alexandria
                #:with-gensyms)
  (:shadow #:get)
  (:export #:start
           #:stop
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
           #:database-name
           ;; Accessor
           #:with-memory-database
           #:get
           #:select
           #:with-current-database
           #:*database*))

(in-package silver-brain.store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Basic                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-dao-class-p (list)
  (every (op (typep _ 'mito:dao-class)) list))

(deftype dao-class-list ()
  `(and list (satisfies every-dao-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Database Accessors                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *database* nil)

(define-condition database-not-found-error (error)
  ((database-name :type string :accessor database-name :initarg :database-name)))

(defmacro with-database ((database-name &key (auto-create nil)
                                          (auto-migrate nil))
                         &body body)
  (with-gensyms (g-database-name)
    `(let* ((,g-database-name ,database-name)
            (*database* ,g-database-name))
       (or ,auto-create
           (string= ":memory:" ,g-database-name)
           (uiop:file-exists-p ,g-database-name)
           (error 'database-not-found-error :database-name ,g-database-name))
       (dbi:with-connection (mito:*connection* :sqlite3
                                               :database-name ,g-database-name)
         (and ,auto-migrate
              (migration:run-migrations))
         ,@body))))

(defmacro with-current-database (&body body)
  `(with-database (*database*)
     ,@body))

(defmacro with-memory-database (&body body)
  `(with-database (":memory:")
     ,@body))

(-> get (symbol string) mito:dao-class)
(defun get (class uuid)
  (mito:find-dao class :uuid uuid))

(defmacro select (class &body clauses)
  `(mito:select-dao ,class ,@clauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             DAO                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mito:deftable concept ()
  ((uuid :col-type :text
         :reader uuid
         :primary-key t)
   (name :col-type :text
         :reader name)
   (content-type :col-type :text :initform ""
                 :reader content-type)
   (content :col-type :text :initform ""
            :reader content))
  (:keys name))

(mito:deftable concept-link ()
  ((uuid :col-type :text
         :reader uuid)
   (source :col-type :text
           :reader source)
   (target :col-type :text
           :reader target))
  (:keys uuid source target))
