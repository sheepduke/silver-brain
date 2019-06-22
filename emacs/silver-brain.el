;;; Package --- silver-brain

;;; Commentary:

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'polymode)
(require 'silver-brain-api)
(require 'silver-brain-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Util                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-default-content-format 'org
  "The default format for concept contents.
Supported values are: plain, markdown, org")

(defvar silver-brain-buffer-name "*Silver Brain*"
  "Buffer name of silver-brain.")

(defvar silver-brain--concept nil
  "The current concept.")

(defvar silver-brain--head-end-point nil
  "The end point of header part, including the separator line.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Modes                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode silver-brain-mode text-mode "Brain"
  "Main mode for single concept Silver Brain."
  )

(define-key silver-brain-mode-map (kbd "<tab>") 'silver-brain-jump-to-next-link)
(define-key silver-brain-mode-map (kbd "<backtab>") 'silver-brain-jump-to-previous-link)
(define-key silver-brain-mode-map (kbd "C-x C-s") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "g") 'silver-brain-refresh)
(define-key silver-brain-mode-map (kbd "r") 'silver-brain-rename)
(define-key silver-brain-mode-map (kbd "s") 'silver-brain-save)
(define-key silver-brain-mode-map (kbd "o") 'silver-brain)
(define-key silver-brain-mode-map (kbd "d") 'silver-brain-delete)
(define-key silver-brain-mode-map (kbd "p") 'silver-brain-add-parent)
(define-key silver-brain-mode-map (kbd "c") 'silver-brain-add-child)
(define-key silver-brain-mode-map (kbd "f") 'silver-brain-add-friend)
(define-key silver-brain-mode-map (kbd "R") 'silver-brain-remove-relation)
(define-key silver-brain-mode-map (kbd "n") 'silver-brain-new-concept)
(define-key silver-brain-mode-map (kbd "q") 'silver-brain-kill-concept)


(add-hook 'silver-brain-mode-hook 'silver-brain--poly-mode)

(run-with-idle-timer 1 t 'silver-brain--auto-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              UI                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun silver-brain--open-concept (uuid)
  "Retrieve given UUID from the server, create and setup the buffer."
  (let ((concept (silver-brain-api--get-concept uuid)))
    (silver-brain-kill-concept)
    (switch-to-buffer silver-brain-buffer-name)
    (silver-brain-mode)
    (silver-brain--setup-buffer concept)))

(defun silver-brain--setup-buffer (concept)
  "Setup buffer with given CONCEPT."
  (setq silver-brain--concept concept)
  (let ((start-point (point)))
    (insert "Concept - " (silver-brain-concept-name concept) "\n")
    (let ((end-point (point)))
      (put-text-property start-point end-point 'font-lock-face 'bold)))
  ;; Draw relations.
  (let* ((uuid (silver-brain-concept-uuid concept))
         (parents (silver-brain-api--get-relation 'parent uuid))
         (children (silver-brain-api--get-relation 'child uuid))
         (friends (silver-brain-api--get-relation 'friend uuid)))
      (silver-brain--draw-relations concept parents children friends))
  (insert "\n" (silver-brain--make-separator))
  ;; Make head part read-only.
  (setq silver-brain--head-end-point (point-max))
  (insert "\n")
  (add-text-properties (point-min) silver-brain--head-end-point
                       '(read-only t silver-brain-relation t))
  (insert (silver-brain-concept-content concept))
  (goto-char (point-min))
  (forward-line)
  (add-hook 'kill-buffer-hook 'silver-brain-confirm-save nil t)
  (set-buffer-modified-p nil))

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
  (let ((name (silver-brain-concept-name concept))
        (start (point)))
    (when (> (length name) (- (window-width) (current-column)))
      (insert "\n"))
    (insert-button (silver-brain-concept-name concept)
                   'help-echo "Open this concept."
                   'action 'silver-brain-follow-link)
    (let ((end (point)))
      (insert " ")
      (put-text-property start end 'uuid (silver-brain-concept-uuid concept)))))

(defun silver-brain-confirm-save ()
  "Confirm to save silver-brain concept."
  (and (buffer-modified-p)
       (y-or-n-p "Current concept is modified; save it? ")
       (silver-brain-save)))

(defun silver-brain--find-concept-buffers ()
  "Return a list of all opened buffers for current concept."
  (remove-if-not (lambda (buffer)
                   (string-match silver-brain-buffer-name (buffer-name buffer)))
                 (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Commands                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun silver-brain ()
  "The entry point of Silver Brain functions."
  (interactive)
  (let* ((uuid (silver-brain-search)))
    (when uuid
      (silver-brain--open-concept uuid))))

;;;###autoload
(defun silver-brain-refresh ()
  "Refresh concept buffer."
  (interactive)
  (silver-brain--open-concept (silver-brain-concept-uuid silver-brain--concept)))

(defun silver-brain--auto-save ()
  "Save silver-brain buffer automatically."
  (when (get-buffer silver-brain-buffer-name)
    (with-current-buffer silver-brain-buffer-name
      (silver-brain-save))))

;;;###autoload
(defun silver-brain-save ()
  "Save silver-brain buffer.
Should be called in a silver-brain-mode buffer."
  (interactive)
  (when (buffer-modified-p)
    (setf (silver-brain-concept-content silver-brain--concept)
          (buffer-substring (1+ silver-brain--head-end-point) (point-max)))
    (silver-brain-api--update-concept silver-brain--concept)
    (set-buffer-modified-p nil)
    (message "Concept updated.")))

;;;###autoload
(defun silver-brain-new-concept ()
  "Create a new concept interactively and open it."
  (interactive)
  (let* ((name (read-string "Concept name: "))
         (content-format silver-brain-default-content-format)
         (concept (silver-brain-api--create-concept name content-format)))
    (silver-brain--open-concept (silver-brain-concept-uuid concept))))

;;;###autoload
(defun silver-brain-kill-concept ()
  "Kill buffers of current Silver Brain concept."
  (interactive)
  (mapcar 'kill-buffer (silver-brain--find-concept-buffers)))

;;;###autoload
(defun silver-brain-rename ()
  "Rename current concept.
Should be called in silver-brain-mode buffers."
  (interactive)
  (let* ((name (read-string "New concept name: "))
         (uuid (silver-brain-concept-uuid silver-brain--concept)))
    (setf (silver-brain-concept-name silver-brain--concept) name)
    (silver-brain-api--update-concept silver-brain--concept)
    (silver-brain--open-concept uuid)))

;;;###autoload
(defun silver-brain-delete ()
  "Delete current concept and all its relations."
  (interactive)
  (when (y-or-n-p "This will delete this concept; confirm? ")
    (silver-brain-api--delete-concept silver-brain--concept)
    (silver-brain-kill-concept)))

;;;###autoload
(defun silver-brain-add-parent ()
  "Add a parent."
  (interactive)
  (silver-brain--add-relation 'parent))

(defun silver-brain-add-child ()
  "Add a child."
  (interactive)
  (silver-brain--add-relation 'child))

;;;###autoload
(defun silver-brain-add-friend ()
  "Add a friend."
  (interactive)
  (silver-brain--add-relation 'friend))

;;;###autoload
(defun silver-brain-remove-relation ()
  "Search and remove relation."
  (interactive)
  (let* ((uuid (silver-brain-concept-uuid silver-brain--concept))
         (parents (silver-brain-api--get-relation 'parent uuid))
         (children (silver-brain-api--get-relation 'child uuid))
         (friends (silver-brain-api--get-relation 'friend uuid))
         (candidates (mapcar #'silver-brain-search--concept-to-candidate
                             (append parents children friends)))
         (selection (completing-read "Select: " candidates))
         (target-uuid (if selection
                          (silver-brain-search--candidate-to-uuid selection)
                        nil)))
    (when target-uuid
      (silver-brain-api--remove-relation
       (cond
        ((member target-uuid (mapcar 'silver-brain-concept-uuid parents)) 'parent)
        ((member target-uuid (mapcar 'silver-brain-concept-uuid children)) 'child)
        ((member target-uuid (mapcar 'silver-brain-concept-uuid friends)) 'friend))
       uuid target-uuid)
      (silver-brain-refresh))))

(defun silver-brain--add-relation (relation)
  "Add relation given by RELATION.
RELATION should be a symbol one of: '(parent child friend)."
  (let* ((target-uuid (silver-brain-search))
         (uuid (silver-brain-concept-uuid silver-brain--concept)))
    (when target-uuid
      (silver-brain-api--add-relation relation uuid target-uuid)
      (silver-brain-refresh))))

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

;; (setq silver-brain-server-port 15000)
;; (global-set-key (kbd "C-c b s") 'silver-brain)
