;; Local Variables:
;; lexical-binding: t
;; nameless-current-name: "silver-brain"
;; End:

(require 'cl-lib)
(require 'silver-brain-vars)

(defvar silver-brain-concept-list '()
  "List of concepts to display.")

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map))

(cl-defmacro silver-brain--with-widget-buffer (buffer-name &body body)
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (let ((inhibit-read-only t))
       (mapc 'widget-delete widget-field-list)
       (erase-buffer)
       ,@body
       (widget-setup)
       (set-buffer-modified-p nil)
       (goto-char (point-min)))))

(defun silver-brain--get-textfield-length (length)
  (max 8 (- (window-width) 10 length)))

(defun alist-set (key alist value)
  (setf (cdr (assoc key alist)) value))

(defun silver-brain--display-time (time-string)
  (format-time-string silver-brain-time-string
                      (encode-time
                       (iso8601-parse time-string))))

(provide 'silver-brain-common)
