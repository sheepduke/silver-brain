(unlisp:defpackage #:silver-brain-tests.common.data.v2
  (:use #:unlisp)
  (:local-nicknames (#:global #:silver-brain.global)
                    (#:v2 #:silver-brain.store.schema.v2)))

(in-package #:silver-brain-tests.common.data.v2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unlisp.dev:setup-package-local-nicknames))

(with-auto-export ()

  ;; Concepts.
  (def concept-editor (make-instance 'v2:concept :uuid "0001" :name "Editor"))

  (def concept-emacs (make-instance 'v2:concept :uuid "0002" :name "Emacs"))

  (def concept-vim (make-instance 'v2:concept :uuid "0003" :name "Vim"))

  (def concept-is-a (make-instance 'v2:concept :uuid "1001" :name "Is a"))

  (def concept-contains (make-instance 'v2:concept :uuid "1002" :name "Contains"))

  (def concept-relates (make-instance 'v2:concept :uuid "1003" :name "Relates to"))

  (def concepts (list concept-editor concept-emacs concept-vim
                      concept-contains concept-relates))

  ;; Aliases.
  (def alias-emacs
    (make-instance 'v2:concept-alias :concept concept-emacs :concept-uuid "0002"
                                     :alias "Editor of Gods"))

  (def alias-vim
    (make-instance 'v2:concept-alias :concept concept-vim :concept-uuid "0003"
                                     :alias "God of Editors"))

  (def aliases (list alias-emacs alias-vim))

  ;; Pairs.
  (def pair-is-a
    (make-instance 'v2:concept-pair :concept concept-is-a :concept-uuid "1001"
                                    :other concept-contains :other-uuid "1002"))

  (def pair-relates
    (make-instance 'v2:concept-pair :concept concept-relates :concept-uuid "1003"
                                    :other concept-relates :other-uuid "1003"))

  (def pairs (list pair-is-a pair-relates))

  ;; Attachments.
  (def attachment-emacs
    (make-instance 'v2:concept-attachment
                   :name "Introduction"
                   :concept concept-emacs :concept-uuid "0002"
                   :content-type "text/org" :content-length 21))

  (def attachment-emacs-content "Emacs, free software.")

  (def attachment-vim
    (make-instance 'v2:concept-attachment
                   :concept concept-vim :concept-uuid "0003"
                   :content-type "text/md" :content-length 13))

  (def attachment-vim-content "Vim 编辑器")
  
  (def attachments (list attachment-emacs attachment-vim))

  ;; Links.
  (def link-emacs-editor
    (make-instance 'v2:concept-link
                   :source concept-emacs :source-uuid "0002"
                   :relation concept-is-a :relation-uuid "1001"
                   :target concept-editor :target-uuid "0001"))

  (def link-editor-vim
    (make-instance 'v2:concept-link
                   :source concept-editor :source-uuid "0001"
                   :relation concept-contains :relation-uuid "1002"
                   :target concept-vim :target-uuid "0003"))

  (def link-emacs-vim
    (make-instance 'v2:concept-link
                   :source concept-editor :source-uuid "0002"
                   :relation concept-relates :relation-uuid "1003"
                   :target concept-vim :target-uuid "0003"))

  (def link (list link-emacs-editor link-editor-vim link-emacs-vim))

  (defun context (fun)
    (silver-brain.store.migration.v2:run)
    (list:foreach concepts #'mito:insert-dao)
    (list:foreach aliases #'mito:insert-dao)
    (list:foreach pairs #'mito:insert-dao)
    (list:foreach attachments #'mito:insert-dao)
    (list:foreach link #'mito:insert-dao)

    (let ((attachment-emacs-path (path:join (global:store/attachments-path
                                             global:*runtime-settings*)
                                            "1-Body.org"))
          (attachment-vim-path (path:join (global:store/attachments-path
                                           global:*runtime-settings*)
                                          "2-Body.md")))
      (io:write-string-into-file attachment-emacs-content attachment-emacs-path
                                 :if-exists :overwrite)
      (io:write-string-into-file attachment-vim-content attachment-vim-path
                                 :if-exists :overwrite))

    (funcall fun)))
