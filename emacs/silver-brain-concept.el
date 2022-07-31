;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-common)
(require 'silver-brain-api)
(require 'silver-brain-core)
(require 'silver-brain-vars)

(defvar silver-brain-concept-buffer-name-format "*Silver Brain Concept %s*")

(defvar silver-brain-alist-content-buffer-name-format "*Silver Brain Concept Content %s*")

(defvar-local silver-brain-current-concept nil)
(put 'silver-brain-current-concept 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-concept-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "e") 'silver-brain-concept-open-content)
    (define-key map (kbd "d") 'silver-brain-delete-this-concept)
    (define-key map (kbd "l i") 'silver-brain-create-inbound-link)
    (define-key map (kbd "l o") 'silver-brain-create-outbound-link)
    (define-key map (kbd "l b") 'silver-brain-create-bidirectional-link)
    map))

(define-derived-mode silver-brain-concept-mode fundamental-mode "SB-Concept"
  "Major mode for Silver Brain single concept.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Buffer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-open (uuid)
  (silver-brain-concept-show (silver-brain-api-get-concept uuid)))

(defun silver-brain-concept-show (concept)
  (let ((buffer (silver-brain--concept-prepare-buffer concept)))
    (pop-to-buffer-same-window buffer)))

(defun silver-brain-concept-refresh ()
  (with-current-buffer (silver-brain--concept-get-buffer-name silver-brain-current-concept)
    (silver-brain--concept-prepare-buffer (silver-brain-api-get-concept (silver-brain-alist-uuid silver-brain-current-concept)))))

(defun silver-brain--concept-prepare-buffer (concept)
  (silver-brain--with-widget-buffer (silver-brain--concept-get-buffer-name concept)
    (silver-brain-concept-mode)
    (setq silver-brain-current-concept concept)
    (setq silver-brain-refresh-function 'silver-brain-concept-refresh)
    (silver-brain--concept-insert-widgets concept))
  (let ((buffer (get-buffer (silver-brain--concept-get-buffer-name concept))))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    buffer))

(defun silver-brain--concept-get-buffer-name (concept)
  (format silver-brain-concept-buffer-name-format (silver-brain-alist-name concept)))

(defun silver-brain-current-concept-name (&rest _)
  (silver-brain-alist-name silver-brain-current-concept))

(defun silver-brain--concept-insert-widgets (concept)
  (silver-brain--widget-insert-with-face "Concept" 'silver-brain-concept-subtitle)
  
  (widget-insert "\n\n  Name: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 6)
                 :value (silver-brain-alist-name concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--rename-concept (widget-value widget))))
  (widget-insert "\n  Content Type: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 14)
                 :value (silver-brain-alist-content-type concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--concept-update-content-type (widget-value widget))))
  (widget-insert "\n  Create Time: ")
  (widget-insert (silver-brain--format-time (silver-brain-alist-create-time concept)))
  (widget-insert "\n  Update Time: ")
  (widget-insert (silver-brain--format-time (silver-brain-alist-update-time concept)))
  (widget-insert "\n\n  ")
  
  ;; Insert buttons.
  (silver-brain--with-push-button-face
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (silver-brain-create-concept))
                  "New")
   (widget-insert " ")
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (silver-brain-delete-this-concept))
                  "Delete")
   (widget-insert " ")
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (kill-buffer))
                  "Back"))

  ;; Insert links.
  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets (silver-brain-alist-inbound-links silver-brain-current-concept)
                                 "Inbound Links"
                                 #'silver-brain-create-inbound-link
                                 #'silver-brain-alist-source
                                 #'silver-brain-alist-source
                                 #'silver-brain-current-concept-name)
  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets (silver-brain-alist-outbound-links silver-brain-current-concept)
                                 "Outbound Links"
                                 #'silver-brain-create-outbound-link
                                 #'silver-brain-alist-target
                                 #'silver-brain-current-concept-name
                                 #'silver-brain-alist-target)
  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets (silver-brain-alist-mutual-links silver-brain-current-concept)
                                 "Mutual Links"
                                 #'silver-brain-alist-mutual-links
                                 #'silver-brain-alist-other
                                 #'silver-brain-current-concept-name
                                 #'silver-brain-alist-other)

  ;; Insert content.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Content" 'silver-brain-concept-subtitle)
  (widget-insert "\n\n")
  (widget-insert (silver-brain-alist-content concept)))

(defun silver-brain--concept-insert-link-widgets
    (links title create-func sort-attr-func source-func target-func)
  "Insert link widgets. LINKS-TYPE is :inbound, :outbound or :mutual."
  (widget-insert "\n")

  ;; Insert sub-title.
  (silver-brain--widget-insert-with-face title
                             'silver-brain-concept-subtitle)
  (widget-insert "  ")

  ;; Insert New button.
  (silver-brain--with-push-button-face
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (funcall create-func))
                  "New"))
  (widget-insert "\n")

  ;; Insert links.
  (let* ((links (sort links
                      (lambda (a b)
                        (string-lessp (silver-brain-alist-name (funcall sort-attr-func a))
                                      (silver-brain-alist-name (funcall sort-attr-func b)))))))
    (and (< 0 (length links)) (widget-insert "\n"))
    (mapc (lambda (link)
            (widget-insert "  ")
            (silver-brain--with-push-button-face
             (widget-create 'push-button
                            :notify (let ((uuid (silver-brain-alist-uuid link)))
                                      (lambda (&rest _)
                                        (silver-brain--concept-confirm-delete-link uuid)))
                            "Unlink"))
            (widget-insert " ")

            (silver-brain--concept-insert-text-or-button (funcall source-func link))
            (widget-insert " → ")
            (silver-brain--concept-insert-text-or-button (silver-brain-alist-relation link)) 
            (widget-insert " → ")
            (silver-brain--concept-insert-text-or-button (funcall target-func link))
            (widget-insert "\n"))
          links)))

(cl-defun silver-brain-create-concept (&optional name)
  "Create a new concept. If NAME is given, it is used as the name
of new concept. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Concept name: ")))
         (concept (silver-brain-api-create-concept name silver-brain-default-content-type)))
    (run-hooks 'silver-brain-after-concept-create-hook)
    (silver-brain-concept-show concept)))

(defun silver-brain-create-inbound-link ()
  (interactive)
  (silver-brain--verify-current-concept)
  (let* (source relation)
    (setq source (silver-brain--search-concept-and-select "Search and select source: "))
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (silver-brain-api-create-link source relation (silver-brain-alist-uuid silver-brain-current-concept) t))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain-create-outbound-link ()
  (interactive)
  (silver-brain--verify-current-concept)
  (let* (relation target)
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (setq target (silver-brain--search-concept-and-select "Search and select target: "))
    (silver-brain-api-create-link (silver-brain-alist-uuid silver-brain-current-concept) relation target t))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain-create-mutual-link ()
  (interactive)
  (silver-brain--verify-current-concept)
  (let* (relation target)
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (setq target (silver-brain--search-concept-and-select "Search and select target: "))
    (silver-brain-api-create-link (silver-brain-alist-uuid silver-brain-current-concept) relation target nil))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-confirm-delete-link (uuid)
  (when (y-or-n-p "Confirm? ")
    (silver-brain-api-delete-link uuid)
    (run-hook-with-args 'silver-brain-after-update-concept-hook)))

(defun silver-brain--concept-insert-text-or-button (string-or-alist)
  "Insert plain text if uuid of CONCEPT-SUMMARY equals to the
current concept, insert a button otherwise."
  (if (typep string-or-alist 'string)
      (silver-brain--widget-insert-with-face (silver-brain-alist-name silver-brain-current-concept)
                                 '(:underline t))
    (let ((uuid (silver-brain-alist-uuid string-or-alist))
          (name (silver-brain-alist-name string-or-alist)))
      (silver-brain--with-concept-hyperlink-face
       (widget-create 'push-button
                      :notify (lambda (&rest _) (silver-brain-concept-open uuid))
                      (if (string-empty-p name) " " name))))))

(defun silver-brain--rename-concept (new-name)
  (let ((concept silver-brain-current-concept))
    (silver-brain-api-update-concept (silver-brain-alist-uuid concept)
                         :name new-name)
    (setf (silver-brain-alist-name silver-brain-current-concept) new-name)
    (rename-buffer (silver-brain--concept-get-buffer-name concept)))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-update-content-type (content-type)
  (silver-brain-api-update-concept (silver-brain-alist-uuid silver-brain-current-concept)
                       :content-type content-type)
  (silver-brain-concept-refresh))

(defun silver-brain-delete-this-concept ()
  (interactive)
  (silver-brain--verify-current-concept)
  (when (y-or-n-p "I will delete this concept and all the related links. Continue?")
    (let ((uuid (silver-brain-alist-uuid silver-brain-current-concept)))
      (kill-buffer)
      (silver-brain-api-delete-concept uuid)
      (run-hooks 'silver-brain-after-delete-concept-hook))))

(defun silver-brain--verify-current-concept ()
  (unless silver-brain-current-concept
    (error "This command must be invoked within a concept buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Content Buffer                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-open-content ()
  (interactive)
  (silver-brain--verify-current-concept)
  (let ((concept silver-brain-current-concept))
    (with-current-buffer (get-buffer-create
                          (format silver-brain-alist-content-buffer-name-format
                                  (silver-brain-alist-name concept)))
      (insert (silver-brain-alist-content concept))

      ;; Decide major mode.
      (funcall (cdr (assoc (silver-brain-alist-content-type concept)
                           silver-brain-content-mode-alist)))
      
      ;; Set local vars.
      (setq silver-brain-current-concept concept)
      
      ;; Set local keys.
      (silver-brain-concept-setup-local-key)

      (set-buffer-modified-p nil)
      (pop-to-buffer-same-window (current-buffer)))))

(defun silver-brain-concept-setup-local-key ()
  (and silver-brain-current-concept
       (let ((keymap (make-sparse-keymap)))
         (set-keymap-parent keymap (current-local-map))
         (use-local-map keymap)
         (define-key keymap (kbd "C-x C-s") 'silver-brain-concept-save-content))))

(defun silver-brain-concept-save-content ()
  (interactive)
  (silver-brain-api-update-concept (silver-brain-alist-uuid silver-brain-current-concept)
                       :content (buffer-string))
  (run-hooks 'silver-brain-after-update-concept-hook)
  (set-buffer-modified-p nil))

(defun silver-brain-concept-refresh-all-buffers ()
  "Refresh all the Silver Brain Concept buffers."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (silver-brain-concept-refresh)))
        (silver-brain-concept-get-all-buffers)))

(defun silver-brain-concept-get-all-buffers ()
  "Return all the Silver Brain Concept buffers."
  (cl-remove-if-not (lambda (buffer)
                      (with-current-buffer buffer
                        (and (string-prefix-p "*Silver Brain Concept"
                                              (buffer-name))
                             (equal 'silver-brain-concept-mode major-mode))))
                    (buffer-list)))

(defun silver-brain--concept-install ()
  "Setup hooks etc for Silver Brain Concept buffers."
  (add-hook 'silver-brain-after-update-concept-hook 'silver-brain-concept-refresh-all-buffers)
  (add-hook 'after-change-major-mode-hook 'silver-brain-concept-setup-local-key))

(provide 'silver-brain-concept)
