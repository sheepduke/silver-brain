;;; Package --- silver-brain-util

;;; Commentary:
;;;
;;; This package provides utility functions for silver-brain related modes.

;;; Code:

;;;###autoload
(cl-defstruct silver-brain-concept uuid name content-format content)

(defun silver-brain--get-link-uuid ()
  "Return the UUID under current link."
  (get-text-property (point) 'uuid))

;;;###autoload
(defun silver-brain-jump-to-next-link ()
  "Jump to next link.
Should be called in a silver-brain-mode buffer."
  (interactive)
  "Jump to next link."
  (let ((next-link-position nil))
    (save-excursion 
      (while (and (< (point) (point-max))
                  (silver-brain--get-link-uuid))
        (forward-char))
      (catch 'found
        (while (< (point) (point-max))
          (when (silver-brain--get-link-uuid)
            (setq next-link-position (point))
            (throw 'found nil))
          (forward-char))))
    (if next-link-position
        (goto-char next-link-position)
      (message "No next link."))))

;;;###autoload
(defun silver-brain-jump-to-previous-link ()
  "Jump to previous link.
Should be called in a silver-brain-mode buffer."
  (interactive)
  (let ((previous-link-position nil))
    (save-excursion
      (while (and (> (point) (point-min))
                  (silver-brain--get-link-uuid))
        (backward-char))
      (catch 'found
        (while (> (point) (point-min))
          (when (silver-brain--get-link-uuid)
            (while (silver-brain--get-link-uuid)
              (backward-char))
            (setq previous-link-position (1+ (point)))
            (throw 'found nil))
          (backward-char))))
    (if previous-link-position
        (goto-char previous-link-position)
      (message "No previous link."))))

(provide 'silver-brain-util)

;;; silver-brain-util.el ends here
