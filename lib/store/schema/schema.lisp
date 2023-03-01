(unlisp:defpackage #:silver-brain.store.schema
  (:use #:unlisp
        #:silver-brain.store.schema.v2)
  (:reexport #:silver-brain.store.schema.v2))

(in-package #:silver-brain.store.schema)

(with-auto-export ()
  (def new-schema-version "V0"))
