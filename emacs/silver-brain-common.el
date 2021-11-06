;; Local Variables:
;; lexical-binding: t
;; nameless-current-name: "silver-brain"
;; End:

(require 'cl-lib)
(require 'silver-brain-vars)

(defvar silver-brain-after-concept-create-hook '())

(defvar silver-brain-after-concept-update-hook '())

(defvar silver-brain-after-concept-delete-hook '())

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map [remap self-insert-command] 'silver-brain-no-edit)
    map))

(defun silver-brain-no-edit ()
  "Invoke button at POS, or refuse to allow editing of Custom buffer."
  (interactive)
  (error "Undefined key binding"))

(defmacro silver-brain--with-widget-buffer (buffer-name &rest body)
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

(defmacro silver-brain-with-concept-hyperlink-face (&rest body)
  `(let ((widget-button-face 'silver-brain-concept-hyperlink)
         (widget-push-button-prefix nil)
         (widget-push-button-suffix nil))
     ,@body))

(defmacro silver-brain-with-push-button-face (&rest body)
  `(let ((widget-button-face 'silver-brain-push-button)
         (widget-push-button-prefix " ")
         (widget-push-button-suffix " "))
     ,@body))

(defun silver-brain-widget-insert-with-face (text face)
  (let ((start (point)))
    (widget-insert text)
    (let ((end (point)))
      (add-face-text-property start end face))))

(provide 'silver-brain-common)
