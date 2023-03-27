(unlisp:defpackage #:silver-brain-tests.common
  (:use #:unlisp
        #:lisp-unit2)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)
                    (#:data.v1 #:silver-brain-tests.common.data.v1)
                    (#:migration.v1 #:silver-brain.store.migration.v1)
                    (#:global #:silver-brain.global)))

(in-package #:silver-brain-tests.common)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()
  (def test-settings
    (make-instance 'global:settings
                   :store/root-path (path:join (path:temporary-directory)
                                               "silver-brain.tests/")))
  (defun test-context (fun)
    (let* ((global:*settings* test-settings)
           (filepath (format nil "~A/~A.sqlite"
                             (global:store/root-path)
                             (uuid:make-v4-uuid))))
      (os:ensure-directories-exist (global:store/root-path))
      (os:ensure-directories-exist (global:store/attachments-path))

      (unwind-protect
           (let ((mito:*connection* nil)
                 (mito:*trace-sql-hooks* nil))
             (dbi:with-connection (mito:*connection* :sqlite3
                                                     :database-name filepath)
               (funcall fun)))
        (os:ensure-file-deleted filepath))))

  (defun run-tests-in-package (package)
    (with-summary ()
      (run-tests :package package)))

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
                               (pack:find-symbol slot-name
                                                 :package package
                                                 :signal-error? t))))

  (defun assert-slots-unbound (object package slot-names)
    (loop for slot-name in slot-names
          do (assert-slot-unbound object package slot-name))))

