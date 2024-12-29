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
    (define-key keymap (kbd "k") #'silver-brain-item-buffer-kill)
    (define-key keymap (kbd "q") #'bury-buffer)
    (define-key keymap (kbd "g") #'silver-brain-item-buffer-refresh)

    (define-key keymap (kbd "e") #'silver-brain-item-edit-content)

    (define-key keymap (kbd "i") #'silver-brain-item-basic-hydra/body)

    (define-key keymap (kbd "SPC") 'silver-brain-item-hydra/body)

    keymap))

(pretty-hydra-define silver-brain-item-hydra (:color blue)
  ("Buffer"
   (("o" silver-brain-search-and-open-item "open"))))

(pretty-hydra-define silver-brain-item-basic-hydra (:color blue)
  ("Modification"
   (("c" silver-brain-create-item "create")
    ("r" silver-brain-item-rename "rename"))))

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
  (when-let (content-buffer (get-buffer silver-brain-item-content-buffer-name))
    (with-current-buffer content-buffer
      (kill-buffer)
      (delete-window)))
  (kill-buffer)
  (pop-to-buffer-same-window silver-brain-list-buffer-name))

(defun silver-brain-item-buffer-edit-content ()
  "Open a new window and show the content there."
  (interactive)
  (silver-brain--verify-current-item)
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
  (silver-brain-item-buffer-refresh))

(defun silver-brain-item-buffer-update-content-type ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((item-id (silver-brain-prop-id silver-brain-current-item))
         (item-name (silver-brain-prop-name silver-brain-current-item))
         (new-name (read-string "New item name: " item-name)))
    (silver-brain-client-update-item (silver-brain-prop-id silver-brain-current-item)
                         :name new-name)))

(defun silver-brain--verify-current-item ()
  (or silver-brain-current-item
      (error "This command must be invoked in the item buffer")))

(provide 'silver-brain-item-buffer)
