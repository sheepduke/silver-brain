(unlisp:defpackage #:silver-brain-tests.concept-map
  (:use #:unlisp
        #:lisp-unit2
        #:silver-brain-tests.common
        #:silver-brain.concept-map)
  (:local-nicknames (#:store #:silver-brain.store)
                    (#:concept-map #:silver-brain.concept-map)
                    (#:data.v2 #:silver-brain-tests.common.data.v2)))

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

(defmethod equal? ((concept concept) (dao store:concept))
  (and (string:= (uuid concept) (store:uuid dao))
       (string:= (name concept) (store:name dao))))

(defmethod equal? ((dao store:concept-attachment) (attachment concept-attachment))
  (and (equal? (id attachment) (mito:object-id dao))
       (equal? (name attachment) (store:name dao))
       (equal? (concept-uuid attachment) (store:concept-uuid dao))
       (equal? (content-type attachment) (store:content-type dao))
       (equal? (content-length attachment) (store:content-length dao))))

(defmethod equal? ((attachment concept-attachment) (dao store:concept-attachment))
  (and (equal? (id attachment) (mito:object-id dao))
       (equal? (name attachment) (store:name dao))
       (equal? (concept-uuid attachment) (store:concept-uuid dao))
       (equal? (content-type attachment) (store:content-type dao))
       (equal? (content-length attachment) (store:content-length dao))))

(define-test load-concept/no-extra-props (:contexts '(test-context
                                                      data.v2:context))
  (let* ((expected data.v2:concept-emacs)
         (actual (load-concept (store:uuid expected))))
    (assert-true (equal? expected actual))
    (assert-concept-map-slots-unbound
     actual '(aliases attachments links linked-concepts created-at updated-at))    ))

(define-test load-concept/attachments (:contexts '(test-context data.v2:context))
  (let* ((uuid (store:uuid data.v2:concept-emacs))
         (concept (load-concept uuid :load-attachments? t))
         (attachments (attachments concept)))
    (assert-equal 1 (list:length attachments))
    (assert-true (equal? data.v2:attachment-emacs
                         (list:first attachments)))))

(define-test load-concept/times (:contexts '(test-context
                                             data.v2:context))
  (let* ((expected data.v2:concept-vim)
         (actual (load-concept (store:uuid expected) :load-times? t)))
    (assert-slots-equal-to-schema expected actual '(uuid name))
    (assert-concept-map-slots-bound actual '(created-at updated-at))
    (assert-concept-map-slots-unbound
     actual '(aliases attachments links linked-concepts))))
