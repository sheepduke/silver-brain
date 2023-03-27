(in-package #:silver-brain.global)

(with-auto-export ()
  (defvar *settings* nil)

  (defclass settings ()
    ((store/root-path :initarg :store/root-path
                      :initform (path:join (path:user-home) "silver-brain.dev/"))))

  (defun store/root-path (&key (settings *settings*))
    (slot-value settings 'store/root-path))

  (defun store/database-path (database-name &key (settings *settings*))
    (path:join (store/root-path :settings settings)
               (format nil "~A.sqlite" database-name)))

  (defun store/attachments-path (&key (settings *settings*))
    (path:join (store/root-path :settings settings)
               "attachments/")))
