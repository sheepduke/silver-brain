(unlisp:defpackage #:silver-brain-tests.common.data.v2
  (:use #:unlisp)
  (:local-nicknames (#:global #:silver-brain.global)
                    (#:migration #:silver-brain.store.migration)
                    (#:v2 #:silver-brain.store.schema.v2)))

(in-package #:silver-brain-tests.common.data.v2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(defun make-concept (uuid name)
  (make-instance 'v2:concept :uuid uuid :name name))

(defun make-alias (concept alias)
  (make-instance 'v2:concept-alias
                 :concept concept
                 :concept-uuid (v2:uuid concept)
                 :alias alias))

(defun make-pair (concept other)
  (make-instance 'v2:concept-pair
                 :concept concept
                 :concept-uuid (v2:uuid concept)
                 :other other
                 :other-uuid (v2:uuid other)))

(defun make-attachment (name concept content-type content-length)
  (make-instance 'v2:concept-attachment
                 :name name
                 :concept concept
                 :concept-uuid (v2:uuid concept)
                 :content-type content-type
                 :content-length content-length))

(defun make-link (source relation target)
  (make-instance 'v2:concept-link
                 :source source :source-uuid (v2:uuid source)
                 :relation relation :relation-uuid (v2:uuid relation)
                 :target target :target-uuid (v2:uuid target)))

(with-auto-export ()
  ;; Concepts.
  (def concept-editor (make-concept "0001" "Editor"))

  (def concept-emacs (make-concept "0002" "Emacs"))

  (def concept-vim (make-concept "0003" "Vim"))

  (def concept-kubernates
    (make-concept "0010" "Kubernates"))

  (def concept-docker (make-concept "0011" "Docker"))

  (def concept-dockerfile (make-concept "0012" "Docker File"))

  (def concept-config-file (make-concept "0013" "Configuration File"))

  (def concept-is-a (make-concept "1001" "Is a"))

  (def concept-contains (make-concept "1002" "Contains"))

  (def concept-relates (make-concept "1003" "Relates to"))

  (def concept-supports (make-concept "1004" "Supports"))

  (def concept-is-supported-by (make-concept "1005" "Is Supported by"))

  (def concepts
    (list concept-editor concept-emacs concept-vim
          concept-kubernates concept-docker
          concept-dockerfile concept-config-file
          concept-contains concept-relates
          concept-supports concept-is-supported-by))

  ;; Aliases.
  (def alias-emacs (make-alias concept-emacs "Editor of Gods"))

  (def alias-vim (make-alias concept-vim "God of Editors"))

  (def alias-kubernates (make-alias concept-kubernates "K8s"))

  (def aliases (list alias-emacs alias-vim alias-kubernates))

  ;; Pairs.
  (def pair-is-a (make-pair concept-is-a concept-contains))

  (def pair-relates (make-pair concept-relates concept-relates))

  (def pair-supports (make-pair concept-supports concept-is-supported-by))

  (def pairs (list pair-is-a pair-relates pair-supports))

  ;; Attachments.
  (def attachment-emacs
    (make-attachment "Introduction" concept-emacs "text/org" 21))

  (def attachment-emacs-content "Emacs, free software.")

  (def attachment-vim1 (make-attachment "Body" concept-vim "text/md" 13))

  (def attachment-vim-content1 "Vim 编辑器")

  (def attachment-vim2 (make-attachment "Body" concept-vim "text/plain" 15))

  (def attachment-vim-content2 "Another content")

  (def attachments (list attachment-emacs attachment-vim1 attachment-vim2))

  ;; Links.
  (def link-emacs-editor (make-link concept-emacs concept-is-a concept-editor))

  (def link-editor-vim (make-link concept-editor concept-contains concept-vim))

  (def link-emacs-vim (make-link concept-emacs concept-relates concept-vim))

  (def link-docker-kubernates
    (make-link concept-docker concept-is-supported-by concept-kubernates))

  (def link-docker-dockerfile
    (make-link concept-docker concept-contains concept-dockerfile))

  (def link-vim-dockerfile
    (make-link concept-vim concept-supports concept-dockerfile))

  (def link (list link-emacs-editor link-editor-vim link-emacs-vim
                  link-docker-kubernates link-docker-dockerfile
                  link-vim-dockerfile))

  (defun context (fun)
    (migration:migrate :upto v2:schema-version)
    (list:foreach concepts #'mito:insert-dao)
    (list:foreach aliases #'mito:insert-dao)
    (list:foreach pairs #'mito:insert-dao)
    (list:foreach attachments #'mito:insert-dao)
    (list:foreach link #'mito:insert-dao)

    (let ((attachment-emacs-path (path:join (global:store/attachments-path
                                             global:*runtime-settings*)
                                            "1-Introduction.org"))
          (attachment-vim-path1 (path:join (global:store/attachments-path
                                            global:*runtime-settings*)
                                           "2-Body.md"))
          (attachment-vim-path2 (path:join (global:store/attachments-path
                                            global:*runtime-settings*)
                                           "3-Body.txt")))
      (loop for pair in (list (cons attachment-emacs-path attachment-emacs-content)
                              (cons attachment-vim-path1 attachment-vim-content1)
                              (cons attachment-vim-path2 attachment-vim-content2))
            do (io:write-string-into-file (cdr pair) (car pair)
                                          :if-does-not-exist :create
                                          :if-exists :overwrite))

      (unwind-protect (funcall fun)
        (loop for file in (list attachment-emacs-path
                                attachment-vim-path1
                                attachment-vim-path2)
              do (os:ensure-file-deleted file))))))
