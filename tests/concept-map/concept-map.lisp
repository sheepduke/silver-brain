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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Tests                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-test get-concept-links/all-props (:contexts '(test-context data:context))
  (let* ((expected-links (pipe (list data:link-emacs-editor
                                     data:link-emacs-vim)
                               (list:sort! :accessor #'mito:object-id)))
         (expected-concepts (list data:concept-vim
                                  data:concept-editor))
         (expected-attachments (list data:attachment-vim1
                                     data:attachment-vim2))
         (concept-links (get-concept-links (store:uuid data:concept-emacs)
                                           :link-level 1
                                           :load-aliases? t
                                           :load-attachments? t
                                           :load-times? t)))
    (assert-true (equal? expected-links (list:sort! (links concept-links)
                                                    :accessor #'id)))

    (loop for expected-concept in expected-concepts
          for uuid = (store:uuid expected-concept)
          for concept = (alist:elt (concepts concept-links) uuid)
          do (assert-slots-equal-to-schema expected-concept concept '(uuid name))
          do (assert-concept-map-slots-bound concept '(created-at updated-at)))

    (assert-true (equal? expected-attachments
                         (attachments (alist:elt (concepts concept-links)
                                                 (store:uuid data:concept-vim)))))))

(define-test get-concept-links/level-n (:contexts '(test-context data:context))
  (let ((link-levels '(2 3 4 5 1000)))
    (loop for link-level in link-levels
          do (let* ((expected-links (pipe (list data:link-vim-dockerfile
                                                data:link-docker-dockerfile
                                                data:link-emacs-vim
                                                data:link-editor-vim
                                                data:link-docker-kubernates)
                                          (list:sort! :accessor #'mito:object-id)))
                    (expected-concepts (list data:concept-vim
                                             data:concept-docker
                                             data:concept-emacs
                                             data:concept-editor
                                             data:concept-kubernates))
                    (concept-links (get-concept-links
                                    (store:uuid data:concept-dockerfile)
                                    :link-level 2)))
               (assert-true (equal? expected-links
                                    (list:sort! (links concept-links)
                                                :accessor #'id)))
               (loop for expected-concept in expected-concepts
                     for uuid = (store:uuid expected-concept)
                     for concept = (alist:elt (concepts concept-links) uuid)
                     do (assert-slots-equal-to-schema
                         expected-concept concept '(uuid name))
                     do (assert-concept-map-slots-unbound
                         concept '(aliases attachments created-at updated-at)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Helpers                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod equal? ((dao store:concept-link) (link concept-link))
  (and (string:= (store:source-uuid dao) (source link))
       (string:= (store:relation-uuid dao) (relation link))
       (string:= (store:target-uuid dao) (target link))))
