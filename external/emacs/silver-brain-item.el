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
    (define-key map (kbd "d") 'silver-brain-delete-this-item)
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
    (silver-brain--item-insert-widgets item)))

(defun silver-brain--item-get-buffer-name (item)
  (format silver-brain-item-buffer-name-format (silver-brain--prop-name item)))

(defun silver-brain--item-insert-widgets (item)
  (silver-brain--widget-insert-with-face (format "Item - %s"
                                     (silver-brain--prop-name silver-brain-current-item))
                             'silver-brain-item-subtitle)
  (widget-insert "\n\n  Name: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 6)
                 :value (silver-brain--prop-name item)
                 :action (lambda (widget &rest _)
                           (silver-brain-item-rename (widget-value widget))))
  (widget-insert "\n  Content Type: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 14)
                 :value (or (silver-brain--prop-content-type item) "") 
                 :action (lambda (widget &rest _)
                           (silver-brain--item-update-content-type (widget-value widget))))
  (widget-insert "\n  Create Time: ")
  (widget-insert (silver-brain--format-time (silver-brain--prop-create-time item)))
  (widget-insert "\n  Update Time: ")
  (widget-insert (silver-brain--format-time (silver-brain--prop-update-time item)))
  (widget-insert "\n\n  ")
  
  ;; Insert buttons.
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
                           (silver-brain-delete-this-item))
                 "Delete")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (kill-buffer))
                 "Close")
  ;; Insert links.
  (widget-insert "\n")
  ;; (silver-brain--item-insert-link-widgets
  ;;  (seq-filter (lambda (link)
  ;;                (and (silver-brain-item-link-directionalp link)
  ;;                     (string-equal (silver-brain-item-summary-id
  ;;                                    (silver-brain-item-link-target link))
  ;;                                   (silver-brain-item-id silver-brain-current-item))))
  ;;              (silver-brain-item-links silver-brain-current-item))
  ;;  :inbound)
  ;; (widget-insert "\n")
  ;; (silver-brain--item-insert-link-widgets
  ;;  (cl-remove-if-not (lambda (link)
  ;;                      (and (silver-brain-item-link-directionalp link)
  ;;                           (string-equal (silver-brain-item-summary-id
  ;;                                          (silver-brain-item-link-source link))
  ;;                                         (silver-brain-item-id silver-brain-current-item))))
  ;;                    (silver-brain-item-links silver-brain-current-item))
  ;;  :outbound)
  ;; (widget-insert "\n")
  ;; (silver-brain--item-insert-link-widgets
  ;;  (cl-remove-if-not (lambda (link)
  ;;                      (not (silver-brain-item-link-directionalp link)))
  ;;                    (silver-brain-item-links silver-brain-current-item))
  ;;  :bidirectional)

  ;; Insert content.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Content" 'silver-brain-item-subtitle)
  (widget-insert "\n\n")
  (widget-insert (or (silver-brain--prop-content item) "")))

;; (defun silver-brain--item-insert-link-widgets (links links-type)
;;   "Insert link widgets. LINKS-TYPE is :inbound, :outbound or :bidirectional."
;;   (widget-insert "\n")

;;   ;; Insert sub-title.
;;   (silver-brain--widget-insert-with-face (format "%s Links" (cl-case links-type
;;                                                   (:inbound "Inbound")
;;                                                   (:outbound "Outbound")
;;                                                   (:bidirectional "Bidirectional")))
;;                              'silver-brain-item-subtitle)
;;   (widget-insert "  ")

;;   ;; Insert New button.
;;   (silver-brain--with-push-button-face
;;    (widget-create 'push-button
;;                   :notify (lambda (&rest _)
;;                             (funcall
;;                              (cl-case links-type
;;                                (:inbound #'silver-brain-create-inbound-link)
;;                                (:outbound #'silver-brain-create-outbound-link)
;;                                (:bidirectional #'silver-brain-create-bidirectional-link))))
;;                   "New"))
;;   (widget-insert "\n")

;;   ;; Insert links.
;;   (let* ((sort-attr (cl-case links-type
;;                       (:inbound #'silver-brain-item-link-source)
;;                       (:outbound #'silver-brain-item-link-target)
;;                       (:bidirectional #'silver-brain-item-link-source)))
;;          (links (sort links
;;                       (lambda (a b)
;;                         (string-lessp (silver-brain-item-summary-name (funcall sort-attr a))
;;                                       (silver-brain-item-summary-name (funcall sort-attr b)))))))
;;     (and (< 0 (length links)) (widget-insert "\n"))
;;     (mapc (lambda (link)
;;             (widget-insert "  ")
;;             (silver-brain--with-push-button-face
;;              (widget-create 'push-button
;;                             :notify (let ((id (silver-brain-item-link-id link)))
;;                                       (lambda (&rest _)
;;                                         (silver-brain--item-confirm-delete-link id)))
;;                             "Unlink"))
;;             (widget-insert " ")

;;             (silver-brain--item-insert-text-or-button (silver-brain-item-link-source link))
;;             (widget-insert " → ")
;;             (silver-brain--item-insert-text-or-button (silver-brain-item-link-relation link)) 
;;             (widget-insert " → ")
;;             (silver-brain--item-insert-text-or-button (silver-brain-item-link-target link))
;;             (widget-insert "\n"))
;;           links)))

(cl-defun silver-brain-create-item (&optional name)
  "Create a new item. If NAME is given, it is used as the name
of new item. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Item name: ")))
         (item (silver-brain-client-create-item name silver-brain-default-content-type)))
    (run-hooks 'silver-brain-after-item-create-hook)
    (silver-brain-item-show item)))

(defun silver-brain-create-inbound-link ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* (source relation)
    (setq source (silver-brain--search-item-and-select "Search and select source: "))
    (setq relation (silver-brain--search-item-and-select "Search and select relation: "))
    (silver-brain-client-create-link source relation (silver-brain-item-id silver-brain-current-item) t))
  (run-hooks 'silver-brain-after-update-item-hook))

(defun silver-brain-create-outbound-link ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* (relation target)
    (setq relation (silver-brain--search-item-and-select "Search and select relation: "))
    (setq target (silver-brain--search-item-and-select "Search and select target: "))
    (silver-brain-client-create-link (silver-brain-item-id silver-brain-current-item) relation target t))
  (run-hooks 'silver-brain-after-update-item-hook))

(defun silver-brain-create-bidirectional-link ()
  (interactive)
  (silver-brain--verify-current-item)
  (let* (relation target)
    (setq relation (silver-brain--search-item-and-select "Search and select relation: "))
    (setq target (silver-brain--search-item-and-select "Search and select target: "))
    (silver-brain-client-create-link (silver-brain-item-id silver-brain-current-item) relation target nil))
  (run-hooks 'silver-brain-after-update-item-hook))

(defun silver-brain--item-confirm-delete-link (id)
  (when (y-or-n-p "Confirm? ")
    (silver-brain-client-delete-link id)
    (run-hook-with-args 'silver-brain-after-update-item-hook)))

(defun silver-brain--item-insert-text-or-button (item-summary)
  "Insert plain text if id of ITEM-SUMMARY equals to the
current item, insert a button otherwise."
  (if (string-equal (silver-brain--prop-id silver-brain-current-item)
                    (silver-brain--prop-id item-summary))
      (silver-brain--widget-insert-with-face (silver-brain-item-summary-name item-summary)
                                 '(:underline t))
    (let ((id (silver-brain-item-summary-id item-summary))
          (name (silver-brain-item-summary-name item-summary)))
      (silver-brain--with-item-hyperlink-face
       (widget-create 'push-button
                      :notify (lambda (&rest _) (silver-brain-item-open id))
                      (if (string-empty-p name) " " name))))))

(defun silver-brain--item-update-content-type (content-type)
  (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                       :content-type content-type)
  (silver-brain-item-refresh))

(defun silver-brain-delete-this-item ()
  (interactive)
  (silver-brain--verify-current-item)
  (when (y-or-n-p "I will delete this item and all the related links. Continue?")
    (let ((id (silver-brain-item-id silver-brain-current-item)))
      (kill-buffer)
      (silver-brain-client-delete-item id)
      (run-hooks 'silver-brain-after-delete-item-hook))))

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

(cl-defun silver-brain-item-rename (&optional new-name)
  (interactive)
  (when (null silver-brain-current-item)
    (error "Must be invoked in an item buffer."))

  (let* ((new-name (or new-name
                      (read-string (format "Rename to: ")
                                   (silver-brain--prop-name silver-brain-current-item))))
         (new-item (silver-brain--update-prop-name new-name)))
    (silver-brain-client-update-item (silver-brain--prop-id)
                         :name new-name)
    (run-hook-with-args 'silver-brain-after-update-item-hook silver-brain-current-item new-item)))

(provide 'silver-brain-item)
