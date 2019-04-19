;;; Package --- silver-brain

;;; Commentary:

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'polymode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Util                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-defstruct silver-brain-concept uuid name content-format content)

(defvar silver-brain-concept-list nil
  "The list of all concepts.")
(defvar silver-brain-default-content-format 'org
  "The default format for concept contents.
Supported values are: plain, markdown, org")
(defvar-local silver-brain--concept nil
  "The concept of current buffer.")
(defvar-local silver-brain--relation-end-point nil
  "The end point of concept relations.")

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
(define-key silver-brain-mode-map (kbd "s") 'silver-brain-select)
(define-key silver-brain-mode-map (kbd "D") 'silver-brain-delete)
(define-key silver-brain-mode-map (kbd "p") 'silver-brain-add-parent)
(define-key silver-brain-mode-map (kbd "P") 'silver-brain-remove-parent)
(define-key silver-brain-mode-map (kbd "c") 'silver-brain-add-child)
(define-key silver-brain-mode-map (kbd "C") 'silver-brain-remove-child)
(define-key silver-brain-mode-map (kbd "f") 'silver-brain-add-friend)
(define-key silver-brain-mode-map (kbd "F") 'silver-brain-remove-friend)
(define-key silver-brain-mode-map (kbd "n") 'silver-brain-new-concept)

(add-hook 'silver-brain-mode-hook 'silver-brain--poly-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              UI                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--open-concept (uuid)
  "Retrieve given UUID from the server, create and setup the buffer."
  (let ((concept (silver-brain-api--get-concept uuid)))
    (when (string= (buffer-name) "*Silver Brain*")
      (kill-buffer))
    (switch-to-buffer "*Silver Brain*")
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
  (setq-local silver-brain--relation-end-point (point-max))
  (insert "\n")
  (add-text-properties (point-min) silver-brain--relation-end-point
                       '(read-only t silver-brain-relation t))
  (insert (silver-brain-concept-content concept))
  (beginning-of-buffer))

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

(defun silver-brain--insert-link (concept)
  "Insert a link according to given CONCEPT into current buffer."
  (let ((start (point)))
    (insert-button (silver-brain-concept-name concept)
                   'help-echo "Open this concept."
                   'action 'silver-brain--follow-link)
    (let ((end (point)))
      (insert " ")
      (put-text-property start end 'uuid (silver-brain-concept-uuid concept)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Commands                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-select ()
  "Select concept and open it."
  (interactive)
  (unless silver-brain-concept-list
    (setq silver-brain-concept-list (silver-brain-api--get-all-concepts)))
  (let ((name (completing-read "Concept: "
                               (mapcar #'silver-brain-concept-name
                                       silver-brain-concept-list))))
    (catch 'found
      (dolist (concept silver-brain-concept-list)
        (when (string= name (silver-brain-concept-name concept))
          (silver-brain--open-concept (silver-brain-concept-uuid concept))
          (throw 'found nil))))))

(defun silver-brain-jump-to-next-link ()
  "Jump to next link.
Should be called in a silver-brain-mode buffer."
  (interactive)
  "Jump to next link or call `org-cycle' according to the context."
  (let ((next-link-position nil))
    (save-excursion 
      (while (and (< (point) silver-brain--relation-end-point)
                  (silver-brain--on-link-p))
        (forward-char))
      (catch 'found
        (while (< (point) silver-brain--relation-end-point)
          (when (silver-brain--on-link-p)
            (setq next-link-position (point))
            (throw 'found nil))
          (forward-char))))
    (if next-link-position
        (goto-char next-link-position)
      (message "No next link."))))

(defun silver-brain-jump-to-previous-link ()
  "Jump to previous link.
Should be called in a silver-brain-mode buffer."
  (interactive)
  (let ((previous-link-position nil))
    (save-excursion
      (while (and (> (point) (point-min))
                  (silver-brain--on-link-p))
        (backward-char))
      (catch 'found
        (while (> (point) (point-min))
          (when (silver-brain--on-link-p)
            (while (silver-brain--on-link-p)
              (backward-char))
            (setq previous-link-position (1+ (point)))
            (throw 'found nil))
          (backward-char))))
    (if previous-link-position
        (goto-char previous-link-position)
      (message "No previous link."))))

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
  (let* ((name (read-string "New concept name: ")))
    (setf (silver-brain-concept-name silver-brain--concept) name)
    (silver-brain-api--update-concept silver-brain--concept)
    (message "Done.")))

(defun silver-brain-delete ()
  "Delete current concept and all its relations."
  (message "TODO"))

(defun silver-brain-add-parent ()
  "Add a parent."
  (interactive)
  (message "TODO"))

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
;;;;                           Utility                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--follow-link (&optional event)
  "Follow a link under current point, EVENT is not used."
  (silver-brain--open-concept (get-text-property (point) 'uuid)))

(defun silver-brain--in-relation-p ()
  "Return T if the current point is in relation part."
  (get-text-property (point) 'silver-brain-relation))

(defun silver-brain--on-link-p ()
  "Return T if the current point is on a link."
  (get-text-property (point) 'uuid))

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
