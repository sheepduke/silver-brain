(defpackage silver-brain.store
  (:use #:cl
        #:silver-brain.util)
  (:local-nicknames (#:config #:silver-brain.config)
                    (#:migration #:silver-brain.store.migration))
  (:import-from #:mito
                #:object-created-at
                #:object-updated-at)
  (:import-from #:serapeum
                #:~>>
                #:op
                #:->)
  (:import-from #:alexandria
                #:with-gensyms)
  (:shadow #:get
           #:count
           #:delete)
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
           #:relation
           #:target
           #:with-database
           #:with-transaction
           #:database-not-found-error
           #:database-name
           ;; Accessor
           #:with-memory-database
           #:get
           #:select
           #:with-current-database
           #:*database*
           #:save
           #:count
           #:delete
           #:delete-by
           #:list-databases))

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

(defun get-database-path (database-name)
  (merge-pathnames database-name (config:data-dir)))

(defmacro with-database ((database-name &key (auto-create nil)
                                          (auto-migrate nil))
                         &body body)
  (with-gensyms (g-database-name)
    `(let* ((,g-database-name (format nil "~a~a.sqlite"
                                      (config:data-dir)
                                      ,database-name))
            (*database* ,g-database-name))
       ,(unless auto-create
          `(or (string= ":memory:" ,g-database-name)
               (uiop:file-exists-p ,g-database-name)
               (error 'database-not-found-error :database-name ,g-database-name)))
       (dbi:with-connection (mito:*connection* :sqlite3
                                               :database-name ,g-database-name)
         (and ,auto-migrate
              (migration:run-migrations))
         ,@body))))

(defmacro with-transaction (&body body)
  `(dbi:with-transaction mito:*connection*
     ,@body))

(defmacro with-current-database (&body body)
  `(with-database (*database*)
     ,@body))

(defmacro with-memory-database (&body body)
  `(with-database (":memory:")
     ,@body))

(-> list-databases () string-list)
(defun list-databases ()
  "Return a list of strings as database."
  (~>> (uiop:directory-files (config:data-dir))
       (remove-if-not (op (string= "sqlite" (pathname-type _))))
       (mapcar (op (pathname-name _)))))

(-> get (symbol string) mito:dao-class)
(defun get (class uuid)
  (mito:find-dao class :uuid uuid))

(-> save (standard-object) t)
(defun save (obj)
  (mito:save-dao obj))

(defmacro select (class &body clauses)
  `(mito:select-dao ,class ,@clauses))

(-> count (symbol &rest t) number)
(defun count (class &rest fields-and-values)
  (apply #'mito:count-dao class fields-and-values))

(-> delete (standard-object) t)
(defun delete (dao)
  (mito:delete-dao dao))

(-> delete-by (symbol &rest t) t)
(defun delete-by (class &rest fields-and-values)
  "Delete by values"
  (apply #'mito:delete-by-values class fields-and-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             DAO                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mito:deftable concept ()
  ((uuid :col-type :text
         :accessor uuid
         :primary-key t)
   (name :col-type :text
         :accessor name)
   (content-type :col-type :text :initform ""
                 :accessor content-type)
   (content :col-type :text :initform ""
            :accessor content))
  (:keys name))

(mito:deftable concept-link ()
  ((source :col-type :text
           :accessor source)
   (relation :col-type :text
             :accessor relation)
   (target :col-type :text
           :accessor target)
   (directionalp :col-type :boolean
                 :accessor directionalp))
  (:keys source relation target))
