;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'avy)
(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-client)

;; ============================================================
;;  Data Model
;; ============================================================

(defun silver-brain--prop-id (&optional obj)
  (silver-brain--prop "id" obj))

(defun silver-brain--prop-name (&optional obj)
  (silver-brain--prop "name" obj))

(defun silver-brain--update-prop-name (value &optional obj)
  (silver-brain--update-prop"name" value obj))

(defun silver-brain--prop-content-type (&optional obj)
  (silver-brain--prop "contentType" obj))

(defun silver-brain--update-prop-content-type (value &optional obj)
  (silver-brain--update-prop "contentType" value obj))

(defun silver-brain--prop-content (&optional obj)
  (silver-brain--prop "content" obj))

(defun silver-brain--prop-parents (&optional obj)
  (silver-brain--prop "parents" obj))

(defun silver-brain--prop-children (&optional obj)
  (silver-brain--prop "children" obj))

(defun silver-brain--prop-siblings (&optional obj)
  (silver-brain--prop "siblings" obj))

(defun silver-brain--prop-references-out (&optional obj)
  (silver-brain--prop "referencesFromThis" obj))

(defun silver-brain--prop-references-in (&optional obj)
  (silver-brain--prop "referencesToThis" obj))

(defun silver-brain--prop-source (&optional obj)
  (silver-brain--prop "source" obj))

(defun silver-brain--prop-target (&optional obj)
  (silver-brain--prop "target" obj))

(defun silver-brain--prop-annotation (&optional obj)
  (silver-brain--prop "annotation" obj))

(defun silver-brain--update-prop-content (value &optional obj)
  (silver-brain--update-prop "content" value obj))

(defun silver-brain--prop-create-time (&optional obj)
  (silver-brain--prop "createTime" obj))

(defun silver-brain--prop-update-time (&optional obj)
  (silver-brain--prop "updateTime" obj))

(defun silver-brain--prop (key &optional obj)
  (let ((obj (or obj silver-brain-current-item)))
    (cdr (assoc-string key obj))))

(defun silver-brain--update-prop (key value &optional obj)
  (let ((obj (or obj silver-brain-current-item)))
    (cons (cons key value)
          (assoc-delete-all key obj #'string=))))

;; ============================================================
;;  Interaction
;; ============================================================

(defvar silver-brain-common-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "j") 'silver-brain-widget-jump)
    (define-key keymap (kbd "q") 'quit-window)
    (define-key keymap (kbd "Q") 'silver-brain-quit-all)
    (define-key keymap (kbd "o") 'silver-brain-open)
    (define-key keymap (kbd "n") 'silver-brain-widget-forward-item)
    (define-key keymap (kbd "p") 'silver-brain-widget-backward-item)
    (define-key keymap (kbd "c i") 'silver-brain-create-item)
    (define-key keymap (kbd "c I") 'silver-brain-create-items)
    (define-key keymap (kbd "d") 'silver-brain-delete-item-at-point)
    (define-key keymap [remap self-insert-command] 'silver-brain--no-edit)
    keymap)
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

(cl-defun silver-brain--search-items-and-select (search-string
                                     &optional (create-if-not-exists-p t))
  "Ask for a search string, search for items and select one.
PROMPT is the prompt for search string."
  (let* ((result (silver-brain-client-search-items search-string))
         (items (seq-map (lambda (item)
                           (cons (format "%s [%s]"
                                         (silver-brain--prop-name item)
                                         (silver-brain--prop-id item))
                                 (silver-brain--prop-id item)))
                         result)))
    (if items
        (cdr (assoc-string (completing-read "Choose item: " items)
                           items))
      (and create-if-not-exists-p
           (y-or-n-p "No item found. Create new one? ")
           (silver-brain-client-create-item (read-string "Name: " search-string)
                                silver-brain-default-content-type)))))

(defun silver-brain-delete-item-at-point ()
  (interactive)
  (let ((item (silver-brain--widget-get-item)))
    (unless item
      (error "Must be invoked upon an item link"))

    (silver-brain--delete-item item)))

(cl-defun silver-brain--delete-items (ids)
  (when (y-or-n-p (format "Delete %d items? " (length ids)))
    (dolist (id ids)
      (silver-brain-client-delete-item id)
      (dolist (buffer (silver-brain--get-all-item-buffers))
        (with-current-buffer buffer
          (if (string= (silver-brain--prop-id) id)
              (kill-buffer)
            (silver-brain-item-refresh))))
      (silver-brain-hello-refresh))))

(cl-defun silver-brain--delete-item (item)
  (when (y-or-n-p (format "Delete item `%s [%s]`?"
                          (silver-brain--prop-name item)
                          (silver-brain--prop-id item)))
    (let ((item-id (silver-brain--prop-id item)))
      (silver-brain-client-delete-item item-id)
      (dolist (buffer (silver-brain--get-all-item-buffers))
        (with-current-buffer buffer
          (if (string= (silver-brain--prop-id) item-id)
              (kill-buffer)
            (silver-brain-item-refresh))))
      (silver-brain-hello-refresh))))

(defun silver-brain--get-all-item-buffers ()
  "Return all the Silver Brain Item buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (string-prefix-p "*Silver Brain Item"
                                        (buffer-name))
                       (equal 'silver-brain-item-mode major-mode))))
              (buffer-list)))

;; ============================================================
;;  Widget Creation
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
       (goto-char (point-min)))
     ,buffer-name))

(defun silver-brain--get-textfield-length (length)
  "Return the width of text field widget. LENGTH is the extra
length to be removed."
  (max 8 (- (min 80 (window-width)) 10 length)))

(defun silver-brain--format-time (timestring)
  "Format given TIMESTRING corresponding to silver-brain-time-format."
  (format-time-string silver-brain-time-format (encode-time (iso8601-parse timestring))))

(defmacro silver-brain--with-item-hyperlink-face (&rest body)
  `(let ((widget-button-face 'silver-brain-item-hyperlink)
         (widget-push-button-prefix nil)
         (widget-push-button-suffix nil))
     ,@body))

(defun silver-brain--widget-create-item (item)
  (let ((widget-button-face 'silver-brain-item-hyperlink)
        (widget-push-button-prefix nil)
        (widget-push-button-suffix nil))
    (let ((widget (widget-create 'push-button
                                 :notify (lambda (&rest _)
                                           (silver-brain-item-open (silver-brain--prop-id item)))
                                 (silver-brain--prop-name item))))
      (widget-put widget 'item item))))

(cl-defun silver-brain--widget-get-item (&optional (point (point)))
  (widget-get (widget-at point) 'item))

(defun silver-brain--widget-create-button (name notify)
  (silver-brain--with-push-button-face 
   (widget-create 'push-button
                  :notify (lambda (&rest _) (funcall notify))
                  name)))

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

;; ============================================================
;;  Widget Movement
;; ============================================================

(defun silver-brain-widget-jump ()
  "Jump to the widgets."
  (interactive)
  (avy-action-goto 
   (avy-process (mapcar (lambda (point)
                          (cons point (selected-window)))
                        (silver-brain--widgets-get-in-buffer)))))

(defun silver-brain-forward-item ()
  (interactive)
  (if-let ((point (silver-brain--widget-next-item))) 
      (progn (goto-char point)
             (message ""))
    (message "No more item")))

(defun silver-brain-backward-item ()
  (interactive)
  (if-let ((point (silver-brain--widget-next-item nil)))
      (progn (goto-char point)
             (message ""))
    (message "No more item")))

(cl-defun silver-brain--widget-next-item (&optional (forwardp t))
  (let ((points (silver-brain--widgets-get-in-buffer))
        (filter-fun (if forwardp #'> #'<)))
    (seq-find (lambda (widget-point)
                (silver-brain--widget-get-item widget-point))
              (seq-filter (lambda (point)
                            (funcall filter-fun point (point)))
                          (if forwardp points (reverse points))))))

(defun silver-brain--widgets-get-in-buffer ()
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

(defun silver-brain--widget-get-item-in-line ()
  (if (silver-brain--widget-has-item-in-line)
      (save-excursion
        (move-end-of-line 0)
        (silver-brain-forward-item)
        (silver-brain--widget-get-item))
    (message "No widget in this line")))

(defun silver-brain--widget-has-item-in-line ()
  (save-excursion
    (let ((line-number (line-number-at-pos)))
      (move-beginning-of-line 1)
      (= line-number (line-number-at-pos (silver-brain--widget-next-item))))))

(provide 'silver-brain-util)
