;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)

(defun silver-brain-item-open-content ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((item silver-brain-current-item))
    (with-current-buffer (get-buffer-create
                          (format silver-brain-item-content-buffer-name-format
                                  (silver-brain--prop-name item)))
      (insert (or (silver-brain--prop-content item) ""))

      ;; Decide major mode.
      (funcall (cdr (assoc (silver-brain--prop-content-type item)
                           silver-brain-content-mode-alist)))
      
      ;; Set local vars.
      (setq silver-brain-current-item item)
      
      ;; Set local keys.
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap (current-local-map))
        (use-local-map keymap)
        (define-key keymap (kbd "C-x C-s") 'silver-brain-item-save-content))

      (set-buffer-modified-p nil)
      (pop-to-buffer-same-window (current-buffer)))))

(defun silver-brain-item-save-content ()
  (interactive)
  (let* ((old-item (seq-copy silver-brain-current-item))
         (new-content (buffer-string))
         (new-item (silver-brain--update-prop-content new-content old-item)))
    (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                         :content new-content)
    (silver-brain-item-refresh-when-id-in (list (silver-brain--prop-id)))
    (set-buffer-modified-p nil)))
