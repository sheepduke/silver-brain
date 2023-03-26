(in-package #:silver-brain.global)

(with-auto-export ()
  (defvar *settings* nil)

  (defclass settings ()
    ((store/root-path :initarg :store/root-path
                      :initform (path:join (path:user-home) "silver-brain.dev/"))))

  (defun store/root-path (&optional (settings *settings*))
    (slot-value settings 'store/root-path))

  (defun store/attachments-path (&optional (settings *settings*))
    (path:join (store/root-path settings)
               "attachments/")))
