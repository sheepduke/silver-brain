(unlisp:defpackage #:silver-brain.server
  (:use #:unlisp)
  (:local-nicknames (#:concept-map #:silver-brain.concept-map)
                    (#:global #:silver-brain.global)
                    (#:store #:silver-brain.store)
                    (#:time #:local-time)))

(in-package #:silver-brain.server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))
