(in-package mitogrator-test)

(defmacro make-migration (name history &key up down)
  (with-gensyms (g-name g-up g-down)
    `(let ((,g-name ,name)
           (,g-up ,up)
           (,g-down ,down))
       (make-instance 'mitogrator:migration
                      :name ,g-name
                      :up (if ,g-up ,g-up (op (push ,g-name ,history)))
                      :down (if ,g-down ,g-down)))))

(defmacro with-mocked-db (latest &body body)
  `(mb:with-dynamic-stubs
       ((mito.connection:connected-p t)
        (mito:ensure-table-exists t)
        (mitogrator::select-latest-migration-history ,latest)
        (mitogrator::insert-migration-history nil))
     (mb:clear-calls)
     ,@body))

(defun make-migration-history (name)
  (make-instance 'mitogrator:migration-history :name name))
