(in-package #:silver-brain.global)

(with-auto-export ()
  (defclass runtime-settings ()
    ((store/root-path :initarg :store/root-path
                      :accessor store/root-path)
     (store/attachments-path :initarg :store/attachments-path
                             :accessor store/attachments-path)))

  (defmethod initialize-instance :after ((object runtime-settings) &key)
    (unless (slot-bound? object 'store/attachments-path)
      (setf (store/attachments-path object)
            (path:join (store/root-path object) "attachments/"))))

  (defvar *runtime-settings*
    (make-instance 'runtime-settings
                   :store/root-path (path:join (path:user-home) ".silver-brain/"))))
