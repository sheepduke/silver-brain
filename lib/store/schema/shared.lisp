(unlisp.prelude:define-package #:silver-brain.store.schema.shared
  (:use #:unlisp.prelude))

(in-package #:silver-brain.store.schema.shared)

(unlisp.dev:setup-package-local-nicknames)

(with-auto-export ()
  (defun make-uuid ()
    (io:write-to-string (uuid:make-v4-uuid)))

  (defgeneric created-at (object)
    (:method (object)
      (mito:object-created-at object)))

  (defgeneric updated-at (object)
    (:method (object)
      (mito:object-updated-at object))))
