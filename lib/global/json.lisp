(in-package #:silver-brain.global)



(with-auto-export ()
  (defun setup ()
    (setf shasht:*symbol-name-function*
          #'slot-name->json-key)
    (unlisp.dev:setup)))
