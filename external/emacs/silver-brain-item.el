;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'dash)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)

;; ============================================================
;;  Mode
;; ============================================================

;; (defvar silver-brain-item-mode-map
;;   (let ((keymap (make-composed-keymap (list (make-sparse-keymap)
;;                                             widget-keymap))))
;;     (define-key keymap (kbd "g") #'silver-brain-item-refresh)
;;     (define-key keymap (kbd "q") #'silver-brain-item-quit)
;;     (define-key keymap (kbd "j") #'silver-brain-widget-jump)

;;     (define-key keymap (kbd "s o") #'silver-brain-search-and-open-item)
;;     (define-key keymap (kbd "s l") #'silver-brain-search)

;;     (define-key keymap (kbd "e") #'silver-brain-item-open-content)
;;     (define-key keymap (kbd "i c") #'silver-brain-item-create-item)
;;     (define-key keymap (kbd "i r") #'silver-brain-item-rename)
;;     (define-key keymap (kbd "i t") #'silver-brain-item-update-content-type)
;;     (define-key keymap (kbd "i d") #'silver-brain-item-delete)

;;     (define-key keymap (kbd "p a") #'silver-brain-item-add-parent)
;;     (define-key keymap (kbd "p c") #'silver-brain-item-create-parent)
;;     (define-key keymap (kbd "p d") #'silver-brain-item-delete-parent)

;;     (define-key keymap (kbd "c a") #'silver-brain-item-add-child)
;;     (define-key keymap (kbd "c c") #'silver-brain-item-create-child)
;;     (define-key keymap (kbd "c d") #'silver-brain-item-delete-child)

;;     (define-key keymap (kbd "r a") #'silver-brain-item-add-reference)
;;     (define-key keymap (kbd "r d") #'silver-brain-item-delete-reference)

;;     (define-key keymap (kbd "SPC") 'silver-brain-item-hydra/body)

;;     keymap))

;; ============================================================
;;  Hotkey
;; ============================================================

;; (pretty-hydra-define silver-brain-item-hydra (:color blue)
;;   ("Buffer"
;;    (("o" silver-brain-search-and-open-item "open")
;;     ("j" silver-brain-widget-jump "jump")
;;     ("g" silver-brain-item-refresh "refresh")
;;     ("q" silver-brain-item-quit "close"))

;;    "Groups"
;;    (("i" silver-brain-item-basic-hydra/body "item")
;;     ("s" silver-brain-item-search-hydra/body "search")
;;     ("p" silver-brain-item-parent-hydra/body "parent")
;;     ("c" silver-brain-item-child-hydra/body "child")
;;     ("r" silver-brain-item-reference-hydra/body "reference"))))

;; (pretty-hydra-define silver-brain-item-basic-hydra ()
;;   ("Item"
;;    (("e" silver-brain-item-open-content "edit content")
;;     ("c" silver-brain-item-create-item "create")
;;     ("r" silver-brain-item-rename "rename")
;;     ("t" silver-brain-item-update-content-type "update content type")
;;     ("d" silver-brain-item-delete "delete this"))))

;; (pretty-hydra-define silver-brain-item-search-hydra ()
;;   ("Search"
;;    (("o" silver-brain-search-and-open-item "open")
;;     ("l" silver-brain-list-items "search"))))

;; (pretty-hydra-define silver-brain-item-parent-hydra ()
;;   ("Parent"
;;    (("a" silver-brain-item-add-parent "add")
;;     ("c" silver-brain-item-create-parent "create")
;;     ("d" silver-brain-item-delete-parent "delete"))))

;; (pretty-hydra-define silver-brain-item-child-hydra ()
;;   ("Child"
;;    (("a" silver-brain-item-add-child "add")
;;     ("c" silver-brain-item-create-child "create")
;;     ("d" silver-brain-item-delete-child "delete"))))

;; (pretty-hydra-define silver-brain-item-reference-hydra ()
;;   ("Reference"
;;    (("a" silver-brain-item-add-reference "add")
;;     ("c" silver-brain-item-create-reference "create")
;;     ("d" silver-brain-item-delete-reference "delete"))))

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

(defun silver-brain-open-item (item-id)
  "Open item with given ITEM-ID."
  (let* ((item (silver-brain-client-get-item item-id))
         (buffer (get-buffer-create silver-brain-item-buffer-name)))
    (silver-brain-nuke-item-content-buffer)
    (with-current-buffer buffer
      (silver-brain-item-buffer-setup item)
      (pop-to-buffer-same-window buffer)
      (silver-brain-item-edit-content))
    (select-window (get-buffer-window buffer))))

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
