;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-vars)
(require 'silver-brain-prop)
(require 'silver-brain-client)

;;;###autoload
(defun silver-brain-open-item-content (item)
  "Display the content of given item. Return the buffer."
  (let* ((buffer (get-buffer-create silver-brain-item-content-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (silver-brain-prop-content item) ""))

      ;; Decide major mode.
      (funcall (cdr (assoc (silver-brain-prop-content-type item)
                           silver-brain-content-mode-alist)))
      
      ;; Set local vars.
      (setq silver-brain-current-item item)
      
      ;; Set local keys.
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap (current-local-map))
        (use-local-map keymap)
        (define-key keymap (kbd "C-x C-s") 'silver-brain-item-content-save))

      (set-buffer-modified-p nil)
      (pop-to-buffer-same-window (current-buffer)))))

(defun silver-brain-item-content-save ()
  (interactive)
  (save-excursion 
    (let* ((old-item (seq-copy silver-brain-current-item))
           (new-content (buffer-string))
           (new-item (silver-brain-prop-update-content new-content old-item)))
      (silver-brain-client-update-item (silver-brain-prop-id silver-brain-current-item)
                           :content new-content)
      (set-buffer-modified-p nil))))

(defun silver-brain-item-content-decide-major-mode ()
  ;; TODO: Implement it.
  )

(provide 'silver-brain-item-content)
