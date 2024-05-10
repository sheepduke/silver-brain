;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)
(require 'silver-brain-item-content)

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
  (silver-brain--widget-create-button "New" (lambda (&rest _) (silver-brain-create-item)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Rename" (lambda (&rest _) (silver-brain-item-rename)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Delete" (lambda (&rest _) (silver-brain-item-delete)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Close" (lambda (&rest _) (kill-buffer)))

  (widget-insert "\n\n"
                 "  Content Type: "
                 (silver-brain--prop-content-type)
                 " ")

  (silver-brain--widget-create-button "Edit" (lambda (&rest _) (silver-brain--item-update-content-type)))

  (widget-insert "\n  Create Time: "
                 (silver-brain--format-time (silver-brain--prop-create-time item))
                 "\n  Update Time: "
                 (silver-brain--format-time (silver-brain--prop-update-time item))
                 "\n")
  
  ;; Insert parents.
  (widget-insert "\n")
  (silver-brain--item-insert-parents-or-children t)
  
  ;; Insert children.
  (widget-insert "\n")
  (silver-brain--item-insert-parents-or-children nil)

  ;; Insert content.
  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Content\n" 'silver-brain-h1)
  (widget-insert (make-horizontal-bar 60)
                 "\n"
                 (or (silver-brain--prop-content item) "")))

(cl-defun make-horizontal-bar (length)
  (string-join (cl-loop for i from 1 to length collect "⎯")))

(defun silver-brain--item-update-content-type ()
  (let ((new-content-type (read-string "Content type: "
                                       (silver-brain--prop-content-type silver-brain-current-item))))
    (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                         :content-type new-content-type)
    (silver-brain-item-refresh)))

(defun silver-brain--verify-current-item ()
  (unless silver-brain-current-item
    (error "This command must be invoked within a item buffer")))

(defun silver-brain--item-insert-parents-or-children (parentp)
  (silver-brain--widget-insert-with-face (if parentp "Parents " "Children ") 'silver-brain-h2)
  (silver-brain--widget-create-button
   "New" (lambda (&rest _)
           (let* ((this-id (silver-brain--prop-id))
                  (other-id (silver-brain--search-items-and-select
                             (read-string (format "Search for %s: "
                                                  (if parentp "parent" "child"))))))
             (if parentp
                 (silver-brain-client-create-child other-id this-id)
               (silver-brain-client-create-child this-id other-id))
             (silver-brain-item-refresh-when-id-in (list this-id other-id)))))

  (let ((others (silver-brain-client-get-items (if parentp
                                       (silver-brain--prop-parents)
                                     (silver-brain--prop-children)))))
    (unless (null others)
      (widget-insert "\n\n"))
    
    (widget-insert "  ")
    
    (dolist (other (silver-brain--item-get-sorted others))
      (silver-brain--widget-create-button
       "Del" (lambda (&rest _)
               (when (y-or-n-p "Confirm? ")
                 (let* ((this-id (silver-brain--prop-id))
                        (other-id (silver-brain--prop-id other)))
                   (if parentp
                       (silver-brain-client-delete-child other-id this-id)
                     (silver-brain-client-delete-child this-id other-id))
                   
                   (silver-brain-item-refresh-when-id-in (list this-id other-id))))))
      (widget-insert " ")
      (silver-brain--widget-create-item other)
      (widget-insert "\n  "))))

(defun silver-brain--item-get-sorted (items)
  (seq-sort-by #'silver-brain--prop-id #'string< 
               (seq-sort-by #'silver-brain--prop-name #'string< items)))

;; ============================================================
;;  Refresh
;; ============================================================

(defun silver-brain-item-refresh-all ()
  (dolist (buffer (silver-brain-item-get-all-item-buffers))
    (with-current-buffer buffer
      (silver-brain-item-refresh))))

(defun silver-brain-item-refresh-when-id-in (ids)
  (dolist (buffer (silver-brain-item-get-all-item-buffers))
    (with-current-buffer buffer
      (when (member (silver-brain--prop-id) ids)
        (silver-brain-item-refresh)))))

(defun silver-brain-item-get-all-item-buffers ()
  "Return all the Silver Brain Item buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (string-prefix-p "*Silver Brain Item"
                                        (buffer-name))
                       (equal 'silver-brain-item-mode major-mode))))
              (buffer-list)))

;; ============================================================
;;  Commands
;; ============================================================

(cl-defun silver-brain-create-item (&optional name)
  "Create a new item. If NAME is given, it is used as the name
of new item. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Item name: ")))
         (item (silver-brain-client-create-item name silver-brain-default-content-type)))
    (silver-brain-hello-refresh)
    (silver-brain-item-show item)))

(cl-defun silver-brain-item-rename ()
  "Rename current item."
  (interactive)
  (silver-brain--verify-current-item)

  (let* ((new-name (read-string (format "Rename to: ")
                                (silver-brain--prop-name silver-brain-current-item)))
         (new-item (silver-brain--update-prop-name new-name)))
    (silver-brain-client-update-item (silver-brain--prop-id)
                         :name new-name)
    (silver-brain-item-refresh-all)
    (silver-brain-hello-refresh)))

(defun silver-brain-item-delete ()
  (interactive)
  (silver-brain--verify-current-item)
  (when (y-or-n-p "I will delete this item and all the related links. Continue?")
    (let ((current-item silver-brain-current-item))
      (silver-brain-client-delete-item (silver-brain--prop-id))
      (kill-buffer)
      (silver-brain-item-refresh-all)
      (silver-brain-hello-refresh))))

(defun silver-brain-item-refresh ()
  "Refresh current item."
  (interactive)
  (let* ((new-item (silver-brain-client-get-item (silver-brain--prop-id)))
         (new-buffer-name (silver-brain--item-get-buffer-name new-item)))
    (rename-buffer new-buffer-name)
    (silver-brain--item-prepare-buffer new-item)))

(provide 'silver-brain-item)
