(unlisp:defpackage #:silver-brain.global
  (:use #:unlisp
        #:chameleon))

(in-package #:silver-brain.global)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))
