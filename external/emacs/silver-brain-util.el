;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'avy)
(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-client)

;; ============================================================
;;  Commands
;; ============================================================

(defun silver-brain-create-and-open-item (&optional item-name)
  "Create a new item and open it."
  (interactive "sNew item name: ")
  (silver-brain-open-item 
   (silver-brain-client-create-item item-name silver-brain-default-content-type)))

(defun silver-brain-search-and-open-item (&optional search-string)
  "Search items with SEARCH-STRING, select it and open it."
  (interactive "sSearch item: ")
  (when-let (item-id (silver-brain-search-and-select-item search-string))
    (silver-brain-open-item item-id)))

(defun silver-brain-search-or-create-item (&optional search-string)
  "Search an item with SEARCH-STRING. If nothing is found, create a new one."
  (interactive "sSearch String: ")
  (if-let (item-id (silver-brain-search-and-select-item search-string))
      item-id
    (call-interactively #'silver-brain-create-item)))

(defun silver-brain-create-item (&optional item-name)
  "Create a new item and return its id."
  (interactive "sNew item name: ")
  (silver-brain-client-create-item item-name silver-brain-default-content-type))

(defun silver-brain-delete-item-at-point ()
  "Delete item defined at point."
  (interactive)
  (if-let (item-id (silver-brain-get-item-id-at-point))
      (if (y-or-n-p (format "Delete item %s [%s]? "
                            (silver-brain-get-item-name-at-point)
                            item-id))
          (silver-brain-client-delete-item item-id)
        (error "Operation canceled"))
    (error "No item is defined at point")))

;; ============================================================
;;  Button & Text
;; ============================================================

(defun silver-brain-insert-item-button (item &optional action)
  "Insert a text button with ITEM name as its label. The ACTION is a
function that takes 0 argument and perform corresponding operation."
  (let ((item-id (silver-brain-prop-id item))
        (item-name (silver-brain-prop-name item)))
    (insert-text-button item-name
                        'action (lambda (_)
                                  (if action
                                      (funcall action)
                                    (silver-brain-open-item item-id)))
                        'item-id item-id
                        'item-name item-name)))

(defun silver-brain-get-item-id-at-point ()
  "Get item id under the point."
  (get-text-property (point) 'item-id))

(defun silver-brain-get-item-name-at-point ()
  "Get item name under the point."
  (get-text-property (point) 'item-name))

(defun silver-brain-insert-h1 (&rest texts)
  "Insert TEXT and apply silver-brain-h1 face to it."
  (apply #'silver-brain-insert-with-face 'silver-brain-h1 texts))

(defun silver-brain-insert-h2 (&rest texts)
  "Insert TEXT and apply silver-brain-h1 face to it."
  (apply #'silver-brain-insert-with-face 'silver-brain-h2 texts))

(defun silver-brain-insert-with-face (face &rest texts)
  "Insert TEXT at point with FACE."
  (let ((start (point)))
    (apply #'insert texts)
    (put-text-property start (point) 'face face)))

;; ============================================================
;;  Item Functions
;; ============================================================

(defun silver-brain-search-and-select-item (search-string)
  "Search items with SEARCH-STRING, select it and return the id."
  (silver-brain-select-item (silver-brain-client-search-items search-string)))

(defun silver-brain-select-item (items)
  "Select one item from given ITEMS."
  (let ((item-map (->> (silver-brain-sort-items items)
                       (--map (cons (format "%s [%s]"
                                            (silver-brain-prop-name it)
                                            (silver-brain-prop-id it))
                                    (silver-brain-prop-id it))))))
    (and items
         (cdr (assoc-string (completing-read "Choose item: " item-map)
                            item-map)))))

(defun silver-brain-open-item (item-id)
  "Open item with given ITEM-ID."
  (let ((item (silver-brain-client-get-item item-id)))
    (pop-to-buffer-same-window (funcall #'silver-brain-open-item-content item))
    (split-window-below)
    (pop-to-buffer-same-window (funcall #'silver-brain-item-buffer-setup item))))

(defun silver-brain-get-item-buffer-name (item-name)
  "Get the name of item buffer with given ITEM-NAME."
  (format "%s%s" silver-brain-item-buffer-name-prefix item-name))

(defun silver-brain-get-item-content-buffer-name (item-name)
  "Get the name of item content buffer."
  (format "%s%s" silver-brain-item-content-buffer-name-prefix item-name))

(defun silver-brain-get-item-buffers ()
  "Return all the Silver Brain Item buffers."
  (->> (buffer-list)
       (-filter (lambda (buffer)
                  (let ((buffer-name (buffer-name buffer)))
                    (or (s-prefix? silver-brain-item-buffer-name-prefix buffer-name)
                        (s-prefix? silver-brain-item-content-buffer-name-prefix buffer-name)))))))

(defun silver-brain-refresh-item-buffers (item-ids)
  (->> (silver-brain-get-item-buffers)
       (--filter (with-current-buffer it
                   ))))

(defun silver-brain-sort-items (items)
  "Sort items."
  (->> items
       (--sort (string< (silver-brain-prop-id it) (silver-brain-prop-id other)))
       (--sort (string< (silver-brain-prop-name it) (silver-brain-prop-name other)))))

(defun silver-brain-format-time (time-string)
  (format-time-string silver-brain-time-format (encode-time (iso8601-parse time-string))))

;; ;; ============================================================
;; ;;  Interaction
;; ;; ============================================================

;; (defun silver-brain--no-edit ()
;;   "Disallow editing custom buffer."
;;   (interactive)
;;   (error "Undefined key binding"))

;; (defun silver-brain-quit-all ()
;;   "Kill all the Silver Brain buffers."
;;   (interactive)
;;   (seq-do (lambda (buffer) (kill-buffer buffer))
;;           (seq-filter (lambda (buffer)
;;                         (string-prefix-p "*Silver Brain" (buffer-name buffer)))
;;                       (buffer-list))))

;; (cl-defun silver-brain--search-items-and-select (search-string)
;;   "Ask for a search string, search for items and select one.
;; PROMPT is the prompt for search string."
;;   (silver-brain--select-item (silver-brain-client-search-items search-string)))

;; (defun silver-brain--select-item (items)
;;   (let* ((sorted-items (silver-brain--get-sorted-items items))
;;          (item-map (seq-map (lambda (item)
;;                               (cons (format "%s [%s]"
;;                                             (silver-brain-prop-name item)
;;                                             (silver-brain-prop-id item))
;;                                     (silver-brain-prop-id item)))
;;                             sorted-items)))
;;     (or (and items
;;              (cdr (assoc-string (completing-read "Choose item: " item-map)
;;                                 item-map)))
;;         (error "No item found"))))

;; (defun silver-brain--get-sorted-items (items)
;;   (seq-sort-by #'silver-brain-prop-name #'string< 
;;                (seq-sort-by #'silver-brain-prop-id #'string< 
;;                             items)))

;; (defun silver-brain-delete-item-at-point ()
;;   (interactive)
;;   (let ((item (silver-brain--widget-get-item)))
;;     (unless item
;;       (error "Must be invoked upon an item link"))

;;     (silver-brain--delete-item item)))

;; (cl-defun silver-brain--create-item ()
;;   (let* ((name (read-string "Item name: ")))
;;     (silver-brain-client-create-item name silver-brain-default-content-type)))

;; (cl-defun silver-brain--delete-items (ids)
;;   (when (y-or-n-p (format "Delete %d items? " (length ids)))
;;     (dolist (id ids)
;;       (silver-brain-client-delete-item id)
;;       (dolist (buffer (silver-brain--get-all-item-buffers))
;;         (with-current-buffer buffer
;;           (if (string= (silver-brain-prop-id) id)
;;               (kill-buffer)
;;             (silver-brain-item-refresh))))
;;       (silver-brain-hello-refresh))))

;; (cl-defun silver-brain--delete-item (item)
;;   (when (y-or-n-p (format "Delete item `%s [%s]`?"
;;                           (silver-brain-prop-name item)
;;                           (silver-brain-prop-id item)))
;;     (let ((item-id (silver-brain-prop-id item)))
;;       (silver-brain-client-delete-item item-id)
;;       (dolist (buffer (silver-brain--get-all-item-buffers))
;;         (with-current-buffer buffer
;;           (if (string= (silver-brain-prop-id) item-id)
;;               (kill-buffer)
;;             (silver-brain-item-refresh))))
;;       (silver-brain-hello-refresh))))

;; (defun silver-brain--get-all-item-buffers ()
;;   "Return all the Silver Brain Item buffers."
;;   (seq-filter (lambda (buffer)
;;                 (with-current-buffer buffer
;;                   (and (string-prefix-p "*SB/Item"
;;                                         (buffer-name))
;;                        (equal 'silver-brain-item-mode major-mode))))
;;               (buffer-list)))

;; ;; ============================================================
;; ;;  Widget Creation
;; ;; ============================================================

;; (defmacro silver-brain--with-widget-buffer (buffer-name &rest body)
;;   "Wrap basic buffer setup functions."
;;   (declare (indent defun))
;;   `(with-current-buffer (get-buffer-create ,buffer-name)
;;      (let ((inhibit-read-only t))
;;        (mapc 'widget-delete widget-field-list)
;;        (erase-buffer)
;;        ,@body
;;        (widget-setup)
;;        (set-buffer-modified-p nil)
;;        (goto-char (point-min)))
;;      ,buffer-name))

;; (defun silver-brain--get-textfield-length (length)
;;   "Return the width of text field widget. LENGTH is the extra
;; length to be removed."
;;   (max 8 (- (min 80 (window-width)) 10 length)))

;; (defun silver-brain--format-time (timestring)
;;   "Format given TIMESTRING corresponding to silver-brain-time-format."
;;   (format-time-string silver-brain-time-format (encode-time (iso8601-parse timestring))))

;; (defmacro silver-brain--with-item-hyperlink-face (&rest body)
;;   `(let ((widget-button-face 'silver-brain-item-hyperlink)
;;          (widget-push-button-prefix nil)
;;          (widget-push-button-suffix nil))
;;      ,@body))

;; (defun silver-brain--widget-create-item (item)
;;   (let ((widget-button-face 'silver-brain-item-hyperlink)
;;         (widget-push-button-prefix nil)
;;         (widget-push-button-suffix nil))
;;     (let ((widget (widget-create 'push-button
;;                                  :notify (lambda (&rest _)
;;                                            (silver-brain-item-open (silver-brain-prop-id item)))
;;                                  (silver-brain-prop-name item))))
;;       (widget-put widget 'item item))))

;; (cl-defun silver-brain--widget-get-item (&optional (point (point)))
;;   (widget-get (widget-at point) 'item))

;; (defun silver-brain--widget-create-button (name notify)
;;   (silver-brain--with-push-button-face 
;;    (widget-create 'push-button
;;                   :notify (lambda (&rest _) (funcall notify))
;;                   name)))

;; (defmacro silver-brain--with-push-button-face (&rest body)
;;   `(let ((widget-button-face 'silver-brain-push-button)
;;          (widget-push-button-prefix " ")
;;          (widget-push-button-suffix " "))
;;      ,@body))

;; (defun silver-brain--widget-insert-with-face (text face)
;;   (let ((start (point)))
;;     (widget-insert text)
;;     (let ((end (point)))
;;       (add-face-text-property start end face))))

;; ;; ============================================================
;; ;;  Widget Movement
;; ;; ============================================================

;; (defun silver-brain-widget-jump ()
;;   "Jump to the widgets."
;;   (interactive)
;;   (avy-action-goto 
;;    (avy-process (mapcar (lambda (point)
;;                           (cons point (selected-window)))
;;                         (silver-brain--widgets-get-in-buffer)))))

;; (defun silver-brain-widget-forward-item ()
;;   (interactive)
;;   (if-let ((point (silver-brain--widget-next-item))) 
;;       (progn (goto-char point)
;;              (message ""))
;;     (message "No more item")))

;; (defun silver-brain-widget-backward-item ()
;;   (interactive)
;;   (if-let ((point (silver-brain--widget-next-item nil)))
;;       (progn (goto-char point)
;;              (message ""))
;;     (message "No more item")))

;; (cl-defun silver-brain--widget-next-item (&optional (forwardp t))
;;   (let ((points (silver-brain--widgets-get-in-buffer))
;;         (filter-fun (if forwardp #'> #'<)))
;;     (seq-find (lambda (widget-point)
;;                 (silver-brain--widget-get-item widget-point))
;;               (seq-filter (lambda (point)
;;                             (funcall filter-fun point (point)))
;;                           (if forwardp points (reverse points))))))

;; (defun silver-brain--widgets-get-in-buffer ()
;;   "Get a list of points of widgets."
;;   (let (widget-points)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (widget-at)
;;         (push (point) widget-points))
;;       (widget-forward 1)

;;       (while (and (widget-at)
;;                   (not (member (point) widget-points)))
;;         (push (point) widget-points)
;;         (widget-forward 1)))
;;     (nreverse widget-points)))

;; (defun silver-brain--widget-get-item-in-line ()
;;   (if (silver-brain--widget-has-item-in-line)
;;       (save-excursion
;;         (move-end-of-line 0)
;;         (silver-brain-forward-item)
;;         (silver-brain--widget-get-item))
;;     (message "No widget in this line")))

;; (defun silver-brain--widget-has-item-in-line ()
;;   (save-excursion
;;     (let ((line-number (line-number-at-pos)))
;;       (move-beginning-of-line 1)
;;       (= line-number (line-number-at-pos (silver-brain--widget-next-item))))))

(provide 'silver-brain-util)
