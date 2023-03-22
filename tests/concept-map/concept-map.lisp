(unlisp:defpackage #:silver-brain-tests.concept-map
  (:use #:unlisp
        #:lisp-unit2
        #:silver-brain-tests.common
        #:silver-brain.concept-map)
  (:local-nicknames (#:store #:silver-brain.store)
                    (#:concept-map #:silver-brain.concept-map)
                    (#:data #:silver-brain-tests.common.data.v2)))

(in-package #:silver-brain-tests.concept-map)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(defun assert-slots-equal-to-schema (expected actual slot-names)
  (assert-multiple-slots-equal expected 'silver-brain.store
                               actual 'silver-brain.concept-map
                               slot-names))

(defun assert-concept-map-slots-bound (object slot-names)
  (assert-slots-bound object 'silver-brain.concept-map slot-names))

(defun assert-concept-map-slots-unbound (object slot-names)
  (assert-slots-unbound object 'silver-brain.concept-map slot-names))

(defmethod equal? ((dao store:concept) (concept concept))
  (and (string:= (uuid concept) (store:uuid dao))
       (string:= (name concept) (store:name dao))))

(defmethod equal? ((dao store:concept-attachment) (attachment concept-attachment))
  (and (equal? (id attachment) (mito:object-id dao))
       (equal? (name attachment) (store:name dao))
       (equal? (concept-uuid attachment) (store:concept-uuid dao))
       (equal? (content-type attachment) (store:content-type dao))
       (equal? (content-length attachment) (store:content-length dao))))

(defmethod equal? ((dao store:concept-alias) (alias string))
  (equal? (store:alias dao) alias))

(define-test get-concept/no-extra-props (:contexts '(test-context data:context))
  (let* ((expected data:concept-emacs)
         (actual (get-concept (store:uuid expected))))
    (assert-true (equal? expected actual))
    (assert-concept-map-slots-unbound
     actual '(aliases attachments created-at updated-at))))

(define-test get-concept/aliases (:contexts '(test-context data:context))
  (let* ((uuid (store:uuid data:concept-kubernates))
         (concept (get-concept uuid :load-aliases? t)))
    (assert-true (equal? (list data:alias-kubernates) (aliases concept)))))

(define-test get-concept/attachments (:contexts '(test-context data:context))
  (let ((test-cases `((,data:concept-emacs . (,data:attachment-emacs))
                      (,data:concept-vim . (,data:attachment-vim1
                                            ,data:attachment-vim2)))))
    (loop for test-case in test-cases
          do (let* ((uuid (store:uuid (car test-case)))
                    (concept (get-concept uuid :load-attachments? t)))
               (assert-true (equal? (cdr test-case)
                                    (attachments concept)))))))

(define-test get-concept/times (:contexts '(test-context data:context))
  (let* ((expected data:concept-vim)
         (concept (get-concept (store:uuid expected) :load-times? t)))
    (assert-slots-equal-to-schema expected concept '(uuid name))
    (assert-concept-map-slots-bound concept '(created-at updated-at))
    (assert-concept-map-slots-unbound concept '(aliases attachments))))

(define-test get-concept/all-props (:contexts '(test-context data:context))
  (let* ((expected data:concept-vim)
         (concept (get-concept (store:uuid expected)
                               :load-aliases? t
                               :load-attachments? t
                               :load-times? t)))
    (assert-slots-equal-to-schema expected concept '(uuid name))
    (assert-true (equal? (list data:attachment-vim1 data:attachment-vim2)
                         (attachments concept)))
    (assert-concept-map-slots-bound concept '(created-at updated-at))))

