(unlisp:defpackage #:silver-brain-tests.common.util
  (:use #:unlisp
        #:lisp-unit2)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)
                    (#:migration.v1 #:silver-brain.store.migration.v1)
                    (#:global #:silver-brain.global))
  (:shadow run-tests))

(in-package #:silver-brain-tests.common.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (defun test-context (fun)
    (let* ((root-path (path:join (path:temporary-directory) "silver-brain-tests/"))
           (filepath (format nil "~A/~A.sqlite" root-path (uuid:make-v4-uuid)))
           (global:*runtime-settings* (make-instance 'global:runtime-settings
                                                     :store/root-path root-path)))
      (os:ensure-directories-exist root-path)
      (os:ensure-directories-exist (global:store/attachments-path
                                    global:*runtime-settings*))

      (unwind-protect
           (let ((mito:*connection* nil)
                 (mito:*trace-sql-hooks* nil))
             (dbi:with-connection (mito:*connection* :sqlite3
                                                     :database-name filepath)
               (funcall fun)))
        (os:ensure-file-deleted filepath))))

  (defun assert-slot-value (object slot-name value)
    (assert-true (equal? value (slot-value object slot-name))))

  (defun assert-slots-values (object list)
    (loop for item in list
          do (assert-slot-value object (list:elt list 0) (list:elt list 1))))

  (defun assert-slots-equal (expected-object expected-package
                             actual-object actual-package
                             slot-name)
    (assert-true (equal? (slot-value expected-object
                                     (pack:find-symbol slot-name
                                                       :package expected-package))
                         (slot-value actual-object
                                     (pack:find-symbol slot-name
                                                       :package actual-package)))))

  (defun assert-multiple-slots-equal (expected-object expected-package
                                      actual-object actual-package
                                      slot-names)
    (loop for slot-name in slot-names
          do (assert-slots-equal expected-object expected-package
                                 actual-object actual-package
                                 slot-name)))

  (defun assert-slot-bound (object package slot-name)
    (assert-true (slot-bound? object
                               (pack:find-symbol slot-name :package package))))

  (defun assert-slots-bound (object package slot-names)
    (loop for slot-name in slot-names
          do (assert-slot-bound object package slot-name)))

  (defun assert-slot-unbound (object package slot-name)
    (assert-false (slot-bound? object
                               (pack:find-symbol slot-name :package package))))

  (defun assert-slots-unbound (object package slot-names)
    (loop for slot-name in slot-names
          do (assert-slot-unbound object package slot-name))))
