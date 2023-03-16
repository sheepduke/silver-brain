(unlisp:defpackage #:silver-brain.store.util
  (:use #:unlisp))

(in-package #:silver-brain.store.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))
