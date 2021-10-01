(defpackage silver-brain.globals
  (:use #:cl)
  (:export #:*actor-system*
           #:start))

(in-package silver-brain.globals)

(defvar *actor-system* nil)

(defun start ()
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system))))
