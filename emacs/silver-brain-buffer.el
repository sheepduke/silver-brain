;;; Package --- silver-brain-buffer

;;; Commentary:
;;;
;;; Buffer management of Silver Brain.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Buffer Name & Find                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--concept-buffer-p ()
  "Return T if current buffer is silver brain buffer."
  (string-match silver-brain-buffer-base-name
                (buffer-name)))

(defun silver-brain--make-concept-buffer-name (concept)
  "Return a string as the buffer name for given CONCEPT."
  (concat "*" (silver-brain--make-concept-buffer-base-name concept) "*"))

(defun silver-brain--make-concept-buffer-base-name (concept)
  "Return a string as the basic buffer name for given CONCEPT."
  (concat silver-brain-buffer-base-name " - "
          (silver-brain-concept-name concept)))

(defun silver-brain--current-concept-buffers ()
  "Return a list of all opened buffers for current concept."
  (cl-remove-if-not (lambda (buffer)
                      (string-match (silver-brain--make-concept-buffer-base-name silver-brain--concept)
                                    (buffer-name buffer)))
                    (buffer-list)))

(cl-defun silver-brain--find-concept-buffers (&key (no-editor-p nil))
  "Return a list of all opened buffers for all concept.
If NO-EDITOR-P is T, exclude the side buffers used by poly-mode."
  (remove-if-not (lambda (buffer)
                   (and (string-match silver-brain-buffer-base-name
                                      (buffer-name buffer))
                        (or (not no-editor-p)
                            (string-match "\*$" (buffer-name buffer)))))
                 (buffer-list)))

(defun silver-brain--open-concept (uuid)
  "Retrieve given UUID from the server, create and setup the buffer."
  (let ((concept (silver-brain-api--get-concept uuid)))
    (silver-brain-kill-concept)
    (silver-brain--open-new-concept uuid)))

(defun silver-brain--open-new-concept (uuid)
  "Retrieve given UUID from the server, create and setup the buffer."
  (let ((concept (silver-brain-api--get-concept uuid)))
    (switch-to-buffer (silver-brain--make-concept-buffer-name concept))
    (silver-brain-mode)
    (silver-brain--setup-buffer concept)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Buffer Construction                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'silver-brain-buffer)

;;; silver-brain-buffer.el ends here
