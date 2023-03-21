(unlisp:defpackage #:silver-brain.concept-map
  (:use #:unlisp)
  (:local-nicknames (#:store #:silver-brain.store)))

(in-package #:silver-brain.concept-map)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))
