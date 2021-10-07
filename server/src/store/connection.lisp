(defpackage silver-brain.store.connection
  (:use #:cl)
  (:local-nicknames (#:migration #:silver-brain.store.migration))
  (:import-from #:serapeum
                #:->
                #:do-hash-table
                #:op)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:hash-table-keys)
  (:export #:start
           #:stop
           #:get)
  (:shadow #:get))

(in-package silver-brain.store.connection)

(defvar *db-conns* nil)

(defun start ()
  (unless *db-conns*
    (setf *db-conns*
          (agt:make-agent (op (make-hash-table :test #'equal))))))

(defun stop ()
  (when *db-conns*
    (agt:agent-update *db-conns* #'disconnect-all)
    (agt:agent-stop *db-conns*)
    (setf *db-conns* nil)))

(-> get (string) (or null dbi.driver:dbi-connection))
(defun get (db-name)
  (gethash db-name
           (agt:agent-update-and-get
            *db-conns*
            (op (update-connections db-name _)))))

(defun database-exists-p (database-name)
  (uiop:file-exists-p database-name))

(defun update-connections (database-name connections)
  (when (and (not (gethash database-name connections))
             (database-exists-p database-name))
    (setf (gethash database-name connections)
          (make-new-connection database-name)))
  connections)

(defun make-new-connection (database-name)
  (let* ((connection (dbi:connect :sqlite3 :database-name database-name))
         (mito:*connection* connection))
    (migration:run-migrations)
    connection))

(defun disconnect-all (conns)
  (dolist (db-name (hash-table-keys conns))
    (dbi:disconnect (gethash db-name conns))
    (remhash db-name conns))
  conns)
