;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)

(defvar silver-brain-item-buffer-name-format "*Silver Brain Item - %s*")

(defvar silver-brain-item-content-buffer-name-format "*Silver Brain Item Content - %s*")

(defvar-local silver-brain-current-item nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-item-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "g") 'silver-brain-item-refresh)
    (define-key map (kbd "e") 'silver-brain-item-open-content)
    (define-key map (kbd "d") 'silver-brain-item-delete)
    (define-key map (kbd "r") 'silver-brain-item-rename)
    map))

(define-derived-mode silver-brain-item-mode fundamental-mode "SB/Item"
  "Major mode for Silver Brain single item.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Buffer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-item-open (id)
  (silver-brain-item-show (silver-brain-client-get-item id)))

(defun silver-brain-item-show (item)
  (let ((buffer (silver-brain--item-prepare-buffer item)))
    (pop-to-buffer-same-window buffer)))

(defun silver-brain-item-refresh ()
  "Refresh current item."
  (interactive)
  (let* ((new-item (silver-brain-client-get-item (silver-brain--prop-id)))
         (new-buffer-name (silver-brain--item-get-buffer-name new-item)))
    (rename-buffer new-buffer-name)
    (silver-brain--item-prepare-buffer new-item)))

(defun silver-brain--item-prepare-buffer (item)
  (silver-brain--with-widget-buffer (silver-brain--item-get-buffer-name item)
    (silver-brain-item-mode)
    (setq silver-brain-current-item item)
    (enable-word-wrap)
    (silver-brain--item-insert-widgets item)))

(defun silver-brain--item-get-buffer-name (item)
  (format silver-brain-item-buffer-name-format (silver-brain--prop-name item)))

(defun silver-brain--item-insert-widgets (item)
  (silver-brain--widget-insert-with-face (format "%s "
                                     (silver-brain--prop-name silver-brain-current-item))
                             'silver-brain-h1)
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain-create-item))
                 "New")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain-item-rename))
                 "Rename")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain-item-delete))
                 "Delete")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (kill-buffer))
                 "Close")

  (widget-insert "\n\n"
                 "  Content Type: "
                 (silver-brain--prop-content-type)
                 " ")
  
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain--item-update-content-type))
                 "Edit")

  (widget-insert "\n  Create Time: "
                 (silver-brain--format-time (silver-brain--prop-create-time item))
                 "\n  Update Time: "
                 (silver-brain--format-time (silver-brain--prop-update-time item))
                 "\n\n")
  
  ;; Insert parents.
  (silver-brain--widget-insert-with-face "Parents " 'silver-brain-h2)
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain--search-items-and-select
                            (read-string "Search parent: "))
                           ;; TODO
                           )
                 "New")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           ;; TODO
                           )
                 "Del")
  (let ((parents (silver-brain-client-get-items (silver-brain--prop-parents))))
    
    (widget-insert "  ")
    (dolist (parent parents)
      (silver-brain--with-item-hyperlink-face
       (widget-create 'push-button
                      :notify (lambda (&rest _)
                                (silver-brain-item-open (silver-brain--prop-id parent)))
                      (silver-brain--prop-name parent))
       (widget-insert "  "))))
  
  ;; Insert children.
  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Children " 'silver-brain-h2)
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           ;; TODO
                           )
                 "New")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           ;; TODO
                           )
                 "Del")
  (widget-insert "\n\n")
  (let ((children (silver-brain-client-get-items (silver-brain--prop-children))))
      (widget-insert "  ")
    (dolist (child children)
      (silver-brain--with-item-hyperlink-face
        (widget-create 'push-button
                       :notify (lambda (&rest _)
                                 (silver-brain-item-open (silver-brain--prop-id child)))
                       (silver-brain--prop-name child))
        (widget-insert "  "))))

  ;; Insert content.
  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Content\n" 'silver-brain-h1)
  (widget-insert (make-horizontal-bar 60)
                 "\n"
                 (or (silver-brain--prop-content item) "")))

(cl-defun make-horizontal-bar (length)
  (string-join (cl-loop for i from 1 to length collect "⎯")))

(cl-defun silver-brain-create-item (&optional name)
  "Create a new item. If NAME is given, it is used as the name
of new item. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Item name: ")))
         (item (silver-brain-client-create-item name silver-brain-default-content-type)))
    (run-hook-with-args 'silver-brain-after-item-create-hook item)
    (silver-brain-item-show item)))

(defun silver-brain--item-confirm-delete-link (id)
  (when (y-or-n-p "Confirm? ")
    (silver-brain-client-delete-link id)
    (run-hook-with-args 'silver-brain-after-update-item-hook)))

(defun silver-brain--item-update-content-type ()
  (let ((new-content-type (read-string "Content type: "
                                       (silver-brain--prop-content-type silver-brain-current-item))))
    (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                         :content-type new-content-type)
    (silver-brain-item-refresh)))

(defun silver-brain-item-delete ()
  (interactive)
  (silver-brain--verify-current-item)
  (when (y-or-n-p "I will delete this item and all the related links. Continue?")
    (let ((current-item silver-brain-current-item))
      (silver-brain-client-delete-item (silver-brain--prop-id))
      (kill-buffer)
      (run-hook-with-args 'silver-brain-after-delete-item-hook
                          current-item))))

(defun silver-brain--verify-current-item ()
  (unless silver-brain-current-item
    (error "This command must be invoked within a item buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Content Buffer                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-item-open-content ()
  (interactive)
  (silver-brain--verify-current-item)
  (let ((item silver-brain-current-item))
    (with-current-buffer (get-buffer-create
                          (format silver-brain-item-content-buffer-name-format
                                  (silver-brain--prop-name item)))
      (insert (silver-brain--prop-content item))

      ;; Decide major mode.
      (funcall (cdr (assoc (silver-brain--prop-content-type item)
                           silver-brain-content-mode-alist)))
      
      ;; Set local vars.
      (setq silver-brain-current-item item)
      
      ;; Set local keys.
      (let ((keymap (make-sparse-keymap)))
        (set-keymap-parent keymap (current-local-map))
        (use-local-map keymap)
        (define-key keymap (kbd "C-x C-s") 'silver-brain-item-save-content))

      (set-buffer-modified-p nil)
      (pop-to-buffer-same-window (current-buffer)))))

(defun silver-brain-item-save-content ()
  (interactive)
  (let* ((old-item (seq-copy silver-brain-current-item))
         (new-content (buffer-string))
         (new-item (silver-brain--update-prop-content new-content old-item)))
    (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                         :content new-content)
    (run-hook-with-args 'silver-brain-after-update-item-hook
                        old-item
                        new-item)
    ;; FIXME
    (set-buffer-modified-p nil)))

;; ============================================================
;;  Hooks
;; ============================================================

(defun silver-brain-item-on-item-updated (old-item new-item)
  (dolist (buffer (silver-brain-item-get-all-item-buffers))
    (with-current-buffer buffer
      (when (or (not (string-equal (silver-brain--prop-name old-item) (silver-brain--prop-name new-item)))
              (string-equal (silver-brain--prop-id old-item) (silver-brain--prop-id silver-brain-current-item)))
        (silver-brain-item-refresh)))))

(defun silver-brain-item-get-all-item-buffers ()
  "Return all the Silver Brain Item buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (string-prefix-p "*Silver Brain Item"
                                        (buffer-name))
                       (equal 'silver-brain-item-mode major-mode))))
              (buffer-list)))

(defun silver-brain--item-install ()
  "Setup hooks etc for Silver Brain Item buffers."
  (add-hook 'silver-brain-after-update-item-hook 'silver-brain-item-on-item-updated)
  ;; (add-hook 'after-change-major-mode-hook 'silver-brain-item-setup-local-key)
  )

;; ============================================================
;;  Commands
;; ============================================================

(cl-defun silver-brain-item-rename ()
  (interactive)
  (silver-brain--verify-current-item)

  (let* ((new-name (read-string (format "Rename to: ")
                                (silver-brain--prop-name silver-brain-current-item)))
         (new-item (silver-brain--update-prop-name new-name)))
    (silver-brain-client-update-item (silver-brain--prop-id)
                         :name new-name)
    (run-hook-with-args 'silver-brain-after-update-item-hook silver-brain-current-item new-item)))

(provide 'silver-brain-item)
