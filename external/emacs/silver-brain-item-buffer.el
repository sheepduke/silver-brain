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

(defvar-local silver-brain-item-outbound-references nil)
(put 'silver-brain-item-outbound-references 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-inbound-references nil)
(put 'silver-brain-item-inbound-references 'permanently-enabled-local-variables t)

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
   (("u" #'silver-brain-item-upsert-property "upsert")
    ("d" #'silver-brain-item-delete-property "delete"))))

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
   (("o" #'silver-brain-item-create-outbound-reference "create outbound")
    ("i" #'silver-brain-item-create-inbound-reference "create inbound")
    ("r" #'silver-brain-item-rename-reference "rename")
    ("d" #'silver-brain-item-delete-reference "delete"))))

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

    ;; Fetch references.
    (setq silver-brain-item-outbound-references
          (silver-brain-client-get-references-from-item (silver-brain-prop-id silver-brain-current-item)))

    (setq silver-brain-item-inbound-references
          (silver-brain-client-get-references-to-item (silver-brain-prop-id silver-brain-current-item)))

    ;; Temporally disable read-only state.
    (setq buffer-read-only nil)
    (erase-buffer)

    ;; Insert contents.
    (silver-brain--item-buffer-insert-components)
    (goto-char (point-min))

    ;; Set the final state.
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun silver-brain--item-buffer-insert-components ()
  (silver-brain-insert-h1 (silver-brain-prop-name silver-brain-current-item) "\n")

  ;; Insert basic information.
  (insert "\n" "ID: " (silver-brain-prop-id silver-brain-current-item) "\n"
          "Content Type: " (silver-brain-prop-content-type silver-brain-current-item) "\n"
          "Create Time: " (silver-brain-format-time (silver-brain-prop-create-time silver-brain-current-item)) "\n"
          "Update Time: " (silver-brain-format-time (silver-brain-prop-update-time silver-brain-current-item)) "\n")

  ;; Insert properties.
  (silver-brain-insert-h2 "\n" "Properties:" "\n")
  (dolist (property (silver-brain-prop-properties silver-brain-current-item))
    (insert (silver-brain-prop-key property) ": " (silver-brain-prop-value property))
    (insert "\n"))

  ;; Insert parents.
  (silver-brain-insert-h2 "\n" "Parents:" "\n")
  (dolist (item (silver-brain-prop-parents silver-brain-current-item))
    (silver-brain-insert-item-button item)
    (insert "\n"))
  (insert "\n")

  ;; Insert children.
  (silver-brain-insert-h2 "Children:" "\n")
  (dolist (item (silver-brain-prop-children silver-brain-current-item))
    (silver-brain-insert-item-button item)
    (insert "\n"))
  (insert "\n")

  ;; Insert references.
  (silver-brain-insert-h2 "References:" "\n")
  (dolist (reference silver-brain-item-outbound-references)
    (insert "This"
            " --(" (silver-brain-prop-annotation reference) ")--> ")
    (silver-brain-insert-item-button (silver-brain-prop-target reference))
    (insert "\n"))

  (dolist (reference silver-brain-item-inbound-references)
    (silver-brain-insert-item-button (silver-brain-prop-source reference))
    (insert " --(" (silver-brain-prop-annotation reference) ")--> "
            "This")
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
  (silver-brain-list-refresh)
  (switch-to-buffer silver-brain-item-buffer-name))

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

(defun silver-brain-item-upsert-property ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((key (silver-brain-item-select-property-key t))
        (value (read-string "Value: ")))
    (silver-brain-client-upsert-item-property (silver-brain-prop-id silver-brain-current-item) key value)
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-delete-property ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((item-id (silver-brain-prop-id silver-brain-current-item))
        (key (silver-brain-item-select-property-key nil)))
    (silver-brain-client-delete-item-property item-id key)
    (silver-brain-item-buffer-refresh)))

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
  (when-let (target-item-id (silver-brain-select-item (-union (silver-brain-prop-parents silver-brain-current-item)
                                                  (silver-brain-prop-children silver-brain-current-item))))
    (cond
     ;; If it is a parent.
     ((--first (s-equals? (silver-brain-prop-id it) target-item-id) silver-brain-item-parents)
      (silver-brain-client-delete-child target-item-id (silver-brain-prop-id silver-brain-current-item)))
     ;; If it is a child.
     ((--first (s-equals? (silver-brain-prop-id it) target-item-id) silver-brain-item-children)
      (silver-brain-client-delete-child (silver-brain-prop-id silver-brain-current-item) target-item-id)))
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-create-outbound-reference ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((source (silver-brain-prop-id silver-brain-current-item))
         (target (silver-brain-search-and-select-item (read-string "Search target item: ")))
         (annotation (read-string "Annotation: ")))
    (silver-brain-client-create-reference source target annotation)
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-create-inbound-reference ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((source (silver-brain-search-and-select-item (read-string "Search target item: ")))
         (target (silver-brain-prop-id silver-brain-current-item))
         (annotation (read-string "Annotation: ")))
    (silver-brain-client-create-reference source target annotation)
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-rename-reference ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((reference (silver-brain-item-select-reference)))
    (silver-brain-client-update-reference (silver-brain-prop-id reference)
                              (read-string "New annotation: "
                                           (silver-brain-prop-annotation reference)))
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-delete-reference ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((reference (silver-brain-item-select-reference)))
    (silver-brain-client-delete-reference (silver-brain-prop-id reference))
    (silver-brain-item-buffer-refresh)))

(defun silver-brain-item-select-property-key (allow-custom)
  (completing-read "Property key: "
                   (--map (silver-brain-prop-key it)
                          (silver-brain-prop-properties silver-brain-current-item))
                   nil (not allow-custom)))

(defun silver-brain-item-select-reference ()
  (silver-brain-completing-read (-concat silver-brain-item-outbound-references
                             silver-brain-item-inbound-references)
                    (lambda (reference)
                      (let* ((item-id (silver-brain-prop-id silver-brain-current-item))
                             (source (silver-brain-prop-source reference))
                             (target (silver-brain-prop-target reference)))
                        (format "%s --(%s)--> %s"
                                (if (s-equals? (silver-brain-prop-id source) item-id)
                                    "This"
                                  (silver-brain-prop-name source))
                                (silver-brain-prop-annotation reference)
                                (if (s-equals? (silver-brain-prop-id target) item-id)
                                    "This"
                                  (silver-brain-prop-name target)))))
                    #'identity))

(defun silver-brain--verify-current-item ()
  (or silver-brain-current-item
      (error "This command must be invoked in the item buffer")))

(provide 'silver-brain-item-buffer)
