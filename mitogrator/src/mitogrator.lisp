(defpackage mitogrator
  (:use #:cl)
  (:export #:run
           #:migration
           #:name
           #:up
           #:down
           #:database-not-connected-error
           #:print-object
           #:migration-history
           #:migration-history-name)
  (:import-from #:serapeum
                #:op
                #:->))

(in-package mitogrator)

(define-condition database-not-connected-error (error)
  ((message :type string
            :initarg :message
            :initform "Database not connected. Please call MITO:CONNECT-TOPLEVEL first.")))

(defclass migration ()
  ((name :type string
         :initarg :name
         :initform (error "Migration name must be provided")
         :accessor name)
   (up :type function :initarg :up :initform (op) :accessor up)
   (down :type function :initarg :down :initform (op) :accessor down)))

(mito:deftable migration-history ()
  ((name :col-type :text
         :primary-key t
         :reader name))
  (:table-name "__migration_history"))

(defmethod print-object ((obj migration-history) stream)
  (format stream "#<Migration \"~a\">" (slot-value obj 'name)))

(-> run (list) t)
(defun run (migrations)
  (assert-mito-connected)
  (mito:ensure-table-exists 'migration-history)
  (let* ((latest-history (select-latest-migration-history))
         (pending-migrations (get-pending-migrations migrations latest-history)))
    (dolist (migration pending-migrations)
      (dbi:with-transaction mito:*connection*
        (funcall (up migration))
        (insert-migration-history (name migration))))))

(defun assert-mito-connected ()
  (unless (mito.connection:connected-p)
    (error 'database-not-connected-error)))

(defun select-latest-migration-history ()
  (first (mito:select-dao 'migration-history
           (sxql:order-by (:desc :name))
           (sxql:limit 1))))

(defun get-pending-migrations (migrations latest-history)
  (cond
    ((null latest-history) migrations)
    (t (let ((name (name latest-history)))
         (remove-if (op (string<= (name _) name)) migrations)))))

(defun insert-migration-history (migration-name)
  (mito:insert-dao (make-instance 'migration-history
                                  :name migration-name)))
