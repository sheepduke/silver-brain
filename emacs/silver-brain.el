;;; Package --- silver-brain

;;; Commentary:

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'polymode)
(require 'silver-brain-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Util                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-default-content-format 'org
  "The default format for concept contents.
Supported values are: plain, markdown, org")

(defvar silver-brain-buffer-name "*Silver Brain*"
  "Buffer name of silver-brain.")

(defvar-local silver-brain--concept nil
  "The concept of current buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Modes                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode silver-brain-mode text-mode "Brain"
  "Main mode for single concept Silver Brain."
  )

(define-key silver-brain-mode-map (kbd "<tab>") 'silver-brain-jump-to-next-link)
(define-key silver-brain-mode-map (kbd "<backtab>") 'silver-brain-jump-to-previous-link)
(define-key silver-brain-mode-map (kbd "C-x C-s") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "r") 'silver-brain-rename)
(define-key silver-brain-mode-map (kbd "w") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "s") 'silver-brain-search)
(define-key silver-brain-mode-map (kbd "d") 'silver-brain-delete)
(define-key silver-brain-mode-map (kbd "p") 'silver-brain-add-parent)
(define-key silver-brain-mode-map (kbd "P") 'silver-brain-remove-parent)
(define-key silver-brain-mode-map (kbd "c") 'silver-brain-add-child)
(define-key silver-brain-mode-map (kbd "C") 'silver-brain-remove-child)
(define-key silver-brain-mode-map (kbd "f") 'silver-brain-add-friend)
(define-key silver-brain-mode-map (kbd "F") 'silver-brain-remove-friend)
(define-key silver-brain-mode-map (kbd "n") 'silver-brain-new-concept)
(define-key silver-brain-mode-map (kbd "q") 'bury-buffer)

(add-hook 'silver-brain-mode-hook 'silver-brain--poly-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              UI                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun silver-brain--open-concept (uuid)
  "Retrieve given UUID from the server, create and setup the buffer."
  (let ((concept (silver-brain-api--get-concept uuid)))
    (when (get-buffer silver-brain-buffer-name)
      (kill-buffer silver-brain-buffer-name))
    (switch-to-buffer silver-brain-buffer-name)
    (silver-brain-mode)
    (silver-brain--setup-buffer concept)))

(defun silver-brain--setup-buffer (concept)
  "Setup buffer with given CONCEPT."
  (setq silver-brain--concept concept)
  (let ((start-point (point)))
    (insert "Concept - " (silver-brain-concept-name concept) "\n")
    (let ((end-point (point)))
      (add-face-text-property start-point end-point 'bold)))
  ;; Draw relations.
  (let* ((uuid (silver-brain-concept-uuid concept))
         (parents (silver-brain-api--get-parents uuid))
         (children (silver-brain-api--get-children uuid))
         (friends (silver-brain-api--get-friends uuid)))
      (silver-brain--draw-relations concept parents children friends))
  (insert "\n" (silver-brain--make-separator))
  ;; Make head part read-only.
  (let ((relation-end-point (point-max)))
    (insert "\n")
    (add-text-properties (point-min) relation-end-point
                         '(read-only t silver-brain-relation t)))
  (insert (silver-brain-concept-content concept))
  (goto-char (point-min)))

(defun silver-brain--draw-relations (concept parents children friends)
  "Insert PARENTS, CHILDREN and FRIENDS relations of given CONCEPT."
  (insert "\nParents: ")
  (dolist (parent parents) (silver-brain--insert-link parent))
  (insert "\n\nChildren: ")
  (dolist (child children) (silver-brain--insert-link child))
  (insert "\n\nFriends: ")
  (dolist (friend friends) (silver-brain--insert-link friend)))

(defun silver-brain--make-separator ()
  "Return a string representing the separator between head and body."
  (make-string (window-width) ?-))

(defun silver-brain--make-impossible-matcher ()
  "Return a random string that will never be matched."
  (let ((source "qwertyuiopasdfghjkl;zxcvbnm,./1234567890-=QWERTYUIOPASDFGHJKLZXCVBNM")
        (result ""))
    (dotimes (i 128)
      (setq result
            (concat result
                    (make-string 1 (elt source (random (length source)))))))
    result))

;;;###autoload
(defun silver-brain-follow-link (&optional event)
  "Follow a link under current point, EVENT is not used."
  (interactive)
  (silver-brain--open-concept (get-text-property (point) 'uuid)))

(defun silver-brain--insert-link (concept)
  "Insert a link according to given CONCEPT into current buffer."
  (let ((start (point)))
    (insert-button (silver-brain-concept-name concept)
                   'help-echo "Open this concept."
                   'action 'silver-brain-follow-link)
    (let ((end (point)))
      (insert " ")
      (put-text-property start end 'uuid (silver-brain-concept-uuid concept)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Commands                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-save ()
  "Save silver-brain buffer.
Should be called in a silver-brain-mode buffer."
  (interactive)
  (message "Buffer saved."))

(defun silver-brain-new-concept ()
  "Create a new concept interactively and open it."
  (interactive)
  (let* ((name (read-string "Concept name: "))
         (content-format silver-brain-default-content-format)
         (concept (silver-brain-api--create-concept name content-format)))
    (silver-brain--open-concept (silver-brain-concept-uuid concept))))

(defun silver-brain-rename ()
  "Rename current concept.
Should be called in silver-brain-mode buffers."
  (interactive)
  (let* ((name (read-string "New concept name: "))
         (uuid (silver-brain-concept-uuid silver-brain--concept)))
    (setf (silver-brain-concept-name silver-brain--concept) name)
    (silver-brain-api--update-concept silver-brain--concept)
    (silver-brain--open-concept uuid)))

(defun silver-brain-delete ()
  "Delete current concept and all its relations."
  (interactive)
  (when (y-or-n-p "This will delete this concept. Confirm?")
    (silver-brain-api--delete-concept silver-brain--concept)
    (when (get-buffer silver-brain-buffer-name)
      (kill-buffer silver-brain-buffer-name))))

(defun silver-brain-add-parent ()
  "Add a parent."
  (interactive)
  ;; (let* ((query (read-string "Search parent: "))
  ;;        (uuid (silver-brain-concept-uuid silver-brain--concept)))
  ;;   (silver-brain-search--find-concept-and-return query)
  ;;   (message "UUID: %s Parent: %s" uuid silver-brain-search--selected-uuid)
  ;;   (when silver-brain-search--selected-uuid
  ;;     (silver-brain-api--add-parent uuid silver-brain-search--selected-uuid)))
  )

;; (silver-brain-api--get-all-concepts)

;; (silver-brain-api--add-parent "A14E8F91-FD89-44BD-8336-7FC7CA62B14B"
;;                               "2DB3F8B0-F572-476C-80E3-E01825D02C88")



(defun silver-brain-remove-parent ()
  "Remove a parent."
  (interactive)
  (message "TODO"))

(defun silver-brain-add-child ()
  "Add a child."
  (interactive)
  (message "TODO"))

(defun silver-brain-remove-child ()
  "Remove a child."
  (interactive)
  (message "TODO"))

(defun silver-brain-add-friend ()
  "Add a friend."
  (interactive)
  (message "TODO"))

(defun silver-brain-remove-friend ()
  "Remove a friend."
  (interactive)
  (message "TODO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Multi Mode                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-hostmode silver-brain--hostmode
  :mode 'silver-brain-mode)

(define-innermode silver-brain--org-innermode
  :mode 'org-mode
  :head-matcher (silver-brain--make-separator)
  :tail-matcher (silver-brain--make-impossible-matcher)
  :head-mode 'body
  :tail-mode 'body)

(define-polymode silver-brain--poly-mode
  :hostmode 'silver-brain--hostmode
  :innermodes '(silver-brain--org-innermode))

(provide 'silver-brain)

;;; silver-brain.el ends here

