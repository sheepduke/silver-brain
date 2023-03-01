(unlisp:defpackage #:silver-brain.store.schema.util
  (:use #:unlisp.prelude)
  (:local-nicknames (#:time #:local-time)))

(in-package #:silver-brain.store.schema.util)

(unlisp.dev:setup-package-local-nicknames)

(with-auto-export ()
  (defun make-uuid ()
    (io:write-to-string (uuid:make-v4-uuid)))

  (defmethod equal? ((left time:timestamp) (right time:timestamp))
    (time:timestamp= left right))

  (defmethod less? ((left time:timestamp) (right time:timestamp))
    (time:timestamp< left right)))
