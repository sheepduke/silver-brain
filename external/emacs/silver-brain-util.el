;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)
(require 'iso8601)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-client)

;; ============================================================
;;  Data Model
;; ============================================================

(defun silver-brain--prop-id (obj)
  (silver-brain--prop "id" obj))

(defun silver-brain--prop-name (obj)
  (silver-brain--prop "name" obj))

(defun silver-brain--update-prop-name (obj value)
  (silver-brain--update-prop obj "name" value))

(defun silver-brain--prop-content-type (obj)
  (silver-brain--prop "contentType" obj))

(defun silver-brain--update-prop-content-type (obj value)
  (silver-brain--update-prop obj "content-type" value))

(defun silver-brain--prop-content (obj)
  (silver-brain--prop "content" obj))

(defun silver-brain--update-prop-content (obj value)
  (silver-brain--update-prop obj "content" value))

(defun silver-brain--prop-create-time (obj)
  (silver-brain--prop "createTime" obj))

(defun silver-brain--prop-update-time (obj)
  (silver-brain--prop "updateTime" obj))

(defun silver-brain--prop (key obj)
  (cdr (assoc-string key obj)))

(defun silver-brain--update-prop (obj key value)
  (cons (cons key value)
        (assoc-delete-all key obj #'string=)))

;; ============================================================
;;  Interaction
;; ============================================================

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'silver-brain-widget-jump)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "Q") 'silver-brain-quit-all)
    (define-key map [remap self-insert-command] 'silver-brain--no-edit)
    map)
  "Common keymap shared by all the Silver Brain buffers.")

(defun silver-brain--no-edit ()
  "Disallow editing custom buffer."
  (interactive)
  (error "Undefined key binding"))

(defun silver-brain-quit-all ()
  "Kill all the Silver Brain buffers."
  (interactive)
  (seq-do (lambda (buffer) (kill-buffer buffer))
          (seq-filter (lambda (buffer)
                        (string-prefix-p "*Silver Brain" (buffer-name buffer)))
                      (buffer-list))))

(cl-defun silver-brain--search-items-and-select (search-string)
  "Ask for a search string, search for items and select one.
PROMPT is the prompt for search string."
  (let* ((result (silver-brain-client-search-items search-string))
         (items (seq-map (lambda (item)
                           (cons (silver-brain--prop-name item) (silver-brain--prop-id item)))
                         result)))
    (and items
         (let ((key (completing-read "Choose item: " items)))
           (cdr (assoc-string key items))))))

;; ============================================================
;;  Widget
;; ============================================================

(defmacro silver-brain--with-widget-buffer (buffer-name &rest body)
  "Wrap basic buffer setup functions."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (let ((inhibit-read-only t))
       (mapc 'widget-delete widget-field-list)
       (erase-buffer)
       ,@body
       (widget-setup)
       (set-buffer-modified-p nil)
       (goto-char (point-min)))))

(defun silver-brain--get-textfield-length (length)
  "Return the width of text field widget. LENGTH is the extra
length to be removed."
  (max 8 (- (min 80 (window-width)) 10 length)))

(defun silver-brain--format-time (timestring)
  "Format given TIMESTRING corresponding to silver-brain-time-format."
  (format-time-string silver-brain-time-format (encode-time (iso8601-parse timestring))))

(defun silver-brain--get-widgets ()
  "Get a list of points of widgets."
  (let (widget-points)
    (save-excursion
      (goto-char (point-min))
      (when (widget-at)
        (push (point) widget-points))
      (widget-forward 1)

      (while (and (widget-at)
                  (not (member (point) widget-points)))
        (push (point) widget-points)
        (widget-forward 1)))
    (nreverse widget-points)))

(defun silver-brain-widget-jump ()
  "Jump to the widgets."
  (interactive)
  (avy-action-goto 
   (avy-process (mapcar (lambda (point)
                          (cons point (selected-window)))
                        (silver-brain--get-widgets)))))

;; ============================================================
;;  Buffer
;; ============================================================

(defmacro silver-brain--with-item-hyperlink-face (&rest body)
  `(let ((widget-button-face 'silver-brain-item-hyperlink)
         (widget-push-button-prefix nil)
         (widget-push-button-suffix nil))
     ,@body))

(defmacro silver-brain--with-push-button-face (&rest body)
  `(let ((widget-button-face 'silver-brain-push-button)
         (widget-push-button-prefix " ")
         (widget-push-button-suffix " "))
     ,@body))

(defun silver-brain--widget-insert-with-face (text face)
  (let ((start (point)))
    (widget-insert text)
    (let ((end (point)))
      (add-face-text-property start end face))))

(provide 'silver-brain-util)
