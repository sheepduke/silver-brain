;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'dash)
(require 's)

(require 'silver-brain-vars)
(require 'silver-brain-client)
(require 'silver-brain-util)
(require 'silver-brain-item)
(require 'silver-brain-item-content)

;; ============================================================
;;  Buffer Variables
;; ============================================================

(defvar-local silver-brain-current-item nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-parents nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-children nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-references nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

;; ============================================================
;;  Mode & Keymap
;; ============================================================

(defvar silver-brain-item-mode-map nil)
(setq silver-brain-item-mode-map
      (let ((keymap (make-sparse-keymap)))
        ;; Buffer.
        (define-key keymap (kbd "k") #'silver-brain-item-buffer-kill)
        (define-key keymap (kbd "g") #'silver-brain-item-buffer-refresh)
        (define-key keymap (kbd "o") #'silver-brain-search-and-open-item)

        ;; History movement.
        (define-key keymap (kbd "H") #'silver-brain-open-previous-item)
        (define-key keymap (kbd "L") #'silver-brain-open-next-item)

        ;; Item.
        (define-key keymap (kbd "c") #'silver-brain-create-and-open-item)
        (define-key keymap (kbd "R") #'silver-brain-item-rename)
        (define-key keymap (kbd "e") #'silver-brain-item-edit-content)
        (define-key keymap (kbd "t") #'silver-brain-item-update-content-type)
        (define-key keymap (kbd "d") #'silver-brain-item-delete)

        ;; Hydra.
        (define-key keymap (kbd "SPC") 'silver-brain-item-hydra/body)
        (define-key keymap (kbd "p") 'silver-brain-item-property-hydra/body)
        (define-key keymap (kbd "a") #'silver-brain-item-attachment-hydra/body)
        (define-key keymap (kbd "l") 'silver-brain-item-link-hydra/body)
        (define-key keymap (kbd "r") 'silver-brain-item-reference-hydra/body)

        keymap))

(pretty-hydra-define silver-brain-item-hydra (:color blue)
  ("Buffer"
   (("k" #'silver-brain-item-buffer-kill "kill")
    ("g" #'silver-brain-item-buffer-refresh "refresh")
    ("o" #'silver-brain-search-and-open-item "open"))

   "History"
   (("H" #'silver-brain-open-previous-item "previous")
    ("L" #'silver-brain-open-next-item "next"))

   "Item"
   (("c" #'silver-brain-create-and-open-item "create")
    ("R" #'silver-brain-item-rename "rename")
    ("u" #'silver-brain-item-update-content-type "update content type")
    ("d" #'silver-brain-item-delete "delete"))

   "More"
   (("p" #'silver-brain-item-property-hydra/body "property")
    ("a" #'silver-brain-item-attachment-hydra/body "attachment")
    ("l" #'silver-brain-item-link-hydra/body "link")
    ("r" #'silver-brain-item-reference-hydra/body "reference"))))

(pretty-hydra-define silver-brain-item-property-hydra (:color blue)
  ("Property"
   (("c" nil "create")
    ("d" nil "delete"))))

(pretty-hydra-define silver-brain-item-attachment-hydra (:color blue)
  ("Attachment"
   (("c" nil "create")
    ("r" nil "rename")
    ("d" nil "delete"))))

(pretty-hydra-define silver-brain-item-link-hydra (:color blue)
  ("Link"
   (("p" #'silver-brain-item-add-parent "add parent")
    ("c" #'silver-brain-item-add-child "add child")
    ("d" #'silver-brain-item-delete-link "delete"))))

(pretty-hydra-define silver-brain-item-reference-hydra (:color blue)
  ("Reference"
   (("c" nil "create")
    ("r" nil "rename")
    ("d" nil "delete"))))

(define-derived-mode silver-brain-item-mode special-mode "SB/Item"
  "Major mode for Silver Brain item."
  :keymap silver-brain-item-mode-map)

;; ============================================================
;;  Buffer Setup
;; ============================================================

;;;###autoload
(defun silver-brain-item-buffer-setup (item)
  "Setup the buffer for corresponding ITEM. Return the buffer."
  (with-current-buffer (get-buffer-create silver-brain-item-buffer-name)
    ;; Enable major mode and set variables.
    (silver-brain-item-mode)
    (setq silver-brain-current-item item)

    ;; Initialize parents.
    (setq silver-brain-item-parents (--> (silver-brain-prop-parents item)
                             (silver-brain-client-get-items it)
                             (silver-brain-sort-items it)))

    ;; Initialize children.
    (setq silver-brain-item-children (--> (silver-brain-prop-children item)
                              (silver-brain-client-get-items it)
                              (silver-brain-sort-items it)))

    ;; Initialize references.
    ;; TODO

    ;; Temporally disable read-only state.
    (setq buffer-read-only nil)
    (erase-buffer)

    ;; Insert contents.
    (silver-brain--item-buffer-insert-components)

    ;; Set the final state.
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun silver-brain--item-buffer-insert-components ()
  (silver-brain-insert-h1 (silver-brain-prop-name silver-brain-current-item) "\n")

  ;; Insert basic information.
  (insert "\nContent type: " (silver-brain-prop-content-type silver-brain-current-item) "\n")
  (insert "Create time: "
          (silver-brain-format-time (silver-brain-prop-create-time silver-brain-current-item))
          "\n")
  (insert "Update time: "
          (silver-brain-format-time (silver-brain-prop-update-time silver-brain-current-item))
          "\n")

  ;; Insert parents
  (silver-brain-insert-h2 "\n" "Parents:" "\n")
  (dolist (item silver-brain-item-parents)
    ;; (insert "  ")
    (silver-brain-insert-item-button item)
    (insert "\n"))
  (insert "\n")

  ;; Insert children.
  (silver-brain-insert-h2 "Children:" "\n")
  (dolist (item silver-brain-item-children)
    ;; (insert "  ")
    (silver-brain-insert-item-button item)
    (insert "\n")))

;; ============================================================
;;  Commands
;; ============================================================

(defun silver-brain-item-buffer-refresh ()
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain-item-buffer-setup (silver-brain-client-get-item (silver-brain-prop-id silver-brain-current-item))))

(defun silver-brain-item-buffer-kill ()
  "Kill current buffer and corresponding content buffer."
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain-nuke-item-content-buffer)
  (kill-buffer)
  (pop-to-buffer-same-window silver-brain-list-buffer-name))

(defun silver-brain-item-edit-content ()
  "Open a new window and show the content there."
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain-nuke-item-content-buffer)
  (split-window-below)
  (windmove-down)
  (silver-brain-open-item-content silver-brain-current-item))

(defun silver-brain-item-rename ()
  "Rename current item."
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain-client-update-item (silver-brain-prop-id silver-brain-current-item)
                       :name (read-string "New item name: "
                                          (silver-brain-prop-name silver-brain-current-item)))
  (silver-brain-item-buffer-refresh)
  (silver-brain-list-refresh))

(defun silver-brain-item-update-content-type ()
  "Update content type."
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain-client-update-item (silver-brain-prop-id silver-brain-current-item)
                       :content-type (read-string "New content type: "
                                                  (silver-brain-prop-content-type silver-brain-current-item)))
  (silver-brain-item-buffer-refresh))

(defun silver-brain-item-delete (&optional no-confirm?)
  "Delete current item."
  (interactive)
  (silver-brain--verify-current-item)
  (when (or no-confirm?
            (y-or-n-p (format "Delete this item (%s)? "
                              (silver-brain-prop-name silver-brain-current-item))))
    (let ((item-id (silver-brain-prop-id silver-brain-current-item)))
      (silver-brain-item-buffer-kill)
      (silver-brain-client-delete-item item-id))

    (silver-brain-list-refresh)))

(defun silver-brain-item-add-parent ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((parent (silver-brain-search-or-create-item (read-string "Search for parent: "))))
    (silver-brain-client-add-child parent (silver-brain-prop-id silver-brain-current-item))
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-add-child ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((child (silver-brain-search-or-create-item (read-string "Search for child: "))))
    (silver-brain-client-add-child (silver-brain-prop-id silver-brain-current-item) child)
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-delete-link ()
  (interactive)
  (silver-brain--verify-current-item)
  (when-let (target-item-id (silver-brain-select-item (-union silver-brain-item-parents silver-brain-item-children)))
    (cond
     ;; If it is a parent.
     ((--first (s-equals? (silver-brain-prop-id it) target-item-id) silver-brain-item-parents)
      (silver-brain-client-delete-child target-item-id (silver-brain-prop-id silver-brain-current-item)))
     ;; If it is a child.
     ((--first (s-equals? (silver-brain-prop-id it) target-item-id) silver-brain-item-children)
      (silver-brain-client-delete-child (silver-brain-prop-id silver-brain-current-item) target-item-id)))
    (silver-brain-item-buffer-refresh)))

(defun silver-brain--verify-current-item ()
  (or silver-brain-current-item
      (error "This command must be invoked in the item buffer")))

(provide 'silver-brain-item-buffer)
