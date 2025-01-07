;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'dash)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)
(require 'silver-brain-item-history)

;; ============================================================
;;  Commands
;; ============================================================

(defun silver-brain-create-item (&optional item-name)
  "Create a new item and return its id."
  (interactive "sNew item name: ")
  (silver-brain-client-create-item item-name silver-brain-default-content-type))

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
;;  Functions
;; ============================================================

(defun silver-brain-open-item (item-id &optional no-history)
  "Open item with given ITEM-ID. When NO-HISTORY is non-nil, do not change the history."
  (let* ((item (silver-brain-client-get-item item-id))
         (buffer (get-buffer-create silver-brain-item-buffer-name)))
    (silver-brain-nuke-item-content-buffer)
    (with-current-buffer buffer
      (silver-brain-item-buffer-setup item)
      (pop-to-buffer-same-window buffer)
      (silver-brain-item-edit-content))
    (select-window (get-buffer-window buffer))

    ;; Push the id to the history.
    (unless no-history
      (silver-brain-item-history-add item-id))))

(defun silver-brain-open-previous-item ()
  "Open previous item from the history."
  (interactive)
  (silver-brain-open-item (silver-brain-item-history-move-backward) t))

(defun silver-brain-open-next-item ()
  "Open next item from the history."
  (interactive)
  (silver-brain-open-item (silver-brain-item-history-move-forward) t))

(defun silver-brain-search-and-select-item (search-string)
  "Search items with SEARCH-STRING, select it and return the id."
  (silver-brain-select-item (silver-brain-client-search-items search-string)))

(defun silver-brain-select-item (items)
  "Select one item from given ITEMS and return its id."
  (silver-brain-completing-read (silver-brain-sort-items items)
                    (lambda (item)
                      (format "%s [%s]"
                              (silver-brain-prop-name item)
                              (silver-brain-prop-id item)))
                    (lambda (item) (silver-brain-prop-id item))))

(defun silver-brain-sort-items (items)
  "Sort items."
  (->> items
       (--sort (string< (silver-brain-prop-id it) (silver-brain-prop-id other)))
       (--sort (string< (silver-brain-prop-name it) (silver-brain-prop-name other)))))

(defun silver-brain-nuke-item-content-buffer ()
  (when-let (buffer (get-buffer silver-brain-item-content-buffer-name))
    (when-let (windows (get-buffer-window-list buffer))
      (-each windows #'delete-window))
    (kill-buffer buffer)))

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

(provide 'silver-brain-item)
