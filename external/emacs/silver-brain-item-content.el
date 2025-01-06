;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'dash)
(require 's)

(require 'silver-brain-vars)
(require 'silver-brain-prop)
(require 'silver-brain-client)

;;;###autoload
(defun silver-brain-open-item-content (item)
  "Display the content of given item. Return the buffer."
  (let* ((buffer (get-buffer-create silver-brain-item-content-buffer-name))
         (content-major-mode (silver-brain-item-content-decide-major-mode (silver-brain-prop-content-type silver-brain-current-item))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (silver-brain-prop-content item) ""))

      ;; Apply it.
      (funcall content-major-mode)
      
      ;; Set local vars.
      (setq silver-brain-current-item item)
      
      ;; Set local keys.
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap (current-local-map))
        (use-local-map keymap)
        (define-key keymap (kbd "C-x C-s") 'silver-brain-item-content-save))

      (goto-char (point-min))
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

(defun silver-brain-item-content-decide-major-mode (content-type)
  "Decide the major mode of content buffer according to given CONTENT-TYPE."
  (cdr (--first (-let ((key (car it)))
                  (or (s-equals? key "*")
                      (and (s-starts-with? "*" key)
                           (s-ends-with? (substring key 1) content-type))
                      (and (s-ends-with? "*" key)
                           (s-starts-with? (substring key 0 (1- (length key)))
                                           content-type))
                      (s-equals? key content-type)))
                silver-brain-content-mode-alist)))

(provide 'silver-brain-item-content)
