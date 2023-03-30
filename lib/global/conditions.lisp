(in-package #:silver-brain.global)

(with-auto-export ()
  (defcondition not-found-error ()
      ((message :initarg :message
                :initform ""))))
