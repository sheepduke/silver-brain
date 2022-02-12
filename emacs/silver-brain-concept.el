;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-common)
(require 'silver-brain-api)
(require 'silver-brain-vars)

(defvar silver-brain-concept-buffer-name-format "*Silver Brain Concept %s*")

(defvar silver-brain-concept-content-buffer-name-format "*Silver Brain Concept Content %s*")

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
    (silver-brain--concept-prepare-buffer (silver-brain-api-get-concept (silver-brain-concept-uuid silver-brain-current-concept)))))

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
  (format silver-brain-concept-buffer-name-format (silver-brain-concept-name concept)))

(defun silver-brain--concept-insert-widgets (concept)
  (silver-brain--widget-insert-with-face "Concept" 'silver-brain-concept-subtitle)
  
  (widget-insert "\n\n  Name: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 6)
                 :value (silver-brain-concept-name concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--concept-rename (widget-value widget))))
  (widget-insert "\n  Content Type: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 14)
                 :value (silver-brain-concept-content-type concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--concept-update-content-type (widget-value widget))))
  (widget-insert "\n  Create Time: ")
  (widget-insert (silver-brain--time-to-string (silver-brain-concept-create-time concept)))
  (widget-insert "\n  Update Time: ")
  (widget-insert (silver-brain--time-to-string (silver-brain-concept-update-time concept)))
  (widget-insert "\n\n  ")
  
  ;; Insert buttons.
  (silver-brain--with-push-button-face
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (silver-brain-new-concept))
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

  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets
   (cl-remove-if-not (lambda (link)
                       (and (silver-brain-concept-link-directionalp link)
                            (string-equal (silver-brain-concept-summary-uuid
                                           (silver-brain-concept-link-target link))
                                          (silver-brain-concept-uuid silver-brain-current-concept))))
                 (silver-brain-concept-links silver-brain-current-concept))
   :inbound)
  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets
   (cl-remove-if-not (lambda (link)
                       (and (silver-brain-concept-link-directionalp link)
                            (string-equal (silver-brain-concept-summary-uuid
                                           (silver-brain-concept-link-source link))
                                          (silver-brain-concept-uuid silver-brain-current-concept))))
                     (silver-brain-concept-links silver-brain-current-concept))
   :outbound)
  (widget-insert "\n")
  (silver-brain--concept-insert-link-widgets
   (cl-remove-if-not (lambda (link)
                       (not (silver-brain-concept-link-directionalp link)))
                     (silver-brain-concept-links silver-brain-current-concept))
   :bidirectional)

  ;; Insert content.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Content" 'silver-brain-concept-subtitle)
  (widget-insert "\n\n")
  (widget-insert (silver-brain-concept-content concept)))

(defun silver-brain--concept-insert-link-widgets (links links-type)
  "Insert link widgets. LINKS-TYPE is :inbound, :outbound or :bidirectional."
  (widget-insert "\n")

  ;; Insert sub-title.
  (silver-brain--widget-insert-with-face (format "%s Links" (cl-case links-type
                                                  (:inbound "Inbound")
                                                  (:outbound "Outbound")
                                                  (:bidirectional "Bidirectional")))
                             'silver-brain-concept-subtitle)
  (widget-insert "  ")

  ;; Insert New button.
  (silver-brain--with-push-button-face
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (funcall
                             (cl-case links-type
                               (:inbound #'silver-brain--concept-create-inbound-link)
                               (:outbound #'silver-brain--concept-create-outbound-link)
                               (:bidirectional #'silver-brain--concept-create-bidirectional-link))))
                  "New"))
  (widget-insert "\n")

  ;; Insert links.
  (let* ((sort-attr (cl-case links-type
                      (:inbound #'silver-brain-concept-link-source)
                      (:outbound #'silver-brain-concept-link-target)
                      (:bidirectional #'silver-brain-concept-link-source)))
         (links (sort links
                      (lambda (a b)
                        (string-lessp (silver-brain-concept-summary-name (funcall sort-attr a))
                                      (silver-brain-concept-summary-name (funcall sort-attr b)))))))
    (and (< 0 (length links)) (widget-insert "\n"))
    (mapc (lambda (link)
            (widget-insert "  ")
            (silver-brain--with-push-button-face
             (widget-create 'push-button
                            :notify (let ((uuid (silver-brain-concept-link-uuid link)))
                                      (lambda (&rest _)
                                        (silver-brain--concept-confirm-delete-link uuid)))
                            "Unlink"))
            (widget-insert " ")

            (silver-brain--concept-insert-text-or-button (silver-brain-concept-link-source link))
            (widget-insert " → ")
            (silver-brain--concept-insert-text-or-button (silver-brain-concept-link-relation link)) 
            (widget-insert " → ")
            (silver-brain--concept-insert-text-or-button (silver-brain-concept-link-target link))
            (widget-insert "\n"))
          links)))

(cl-defun silver-brain-new-concept (&optional name)
  "Create a new concept. If NAME is given, it is used as the name
of new concept. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Concept name: ")))
         (concept (silver-brain-api-create-concept name silver-brain-default-content-type)))
    (run-hooks 'silver-brain-after-concept-create-hook)
    (silver-brain-concept-show concept)))

(defun silver-brain--concept-create-inbound-link ()
  (let* (source relation)
    (setq source (silver-brain--search-concept-and-select "Search and select source: "))
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (silver-brain-api-create-link source relation (silver-brain-concept-uuid silver-brain-current-concept) t))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-create-outbound-link ()
  (let* (relation target)
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (setq target (silver-brain--search-concept-and-select "Search and select target: "))
    (silver-brain-api-create-link (silver-brain-concept-uuid silver-brain-current-concept) relation target t))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-create-bidirectional-link ()
  (let* (relation target)
    (setq relation (silver-brain--search-concept-and-select "Search and select relation: "))
    (setq target (silver-brain--search-concept-and-select "Search and select target: "))
    (silver-brain-api-create-link (silver-brain-concept-uuid silver-brain-current-concept) relation target nil))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-confirm-delete-link (uuid)
  (when (y-or-n-p "Confirm? ")
    (silver-brain-delete-link uuid)
    (run-hook-with-args 'silver-brain-after-update-concept-hook)))

(defun silver-brain--concept-insert-text-or-button (concept-summary)
  "Insert plain text if uuid of CONCEPT-SUMMARY equals to the
current concept, insert a button otherwise."
  (if (string-equal (silver-brain-concept-uuid silver-brain-current-concept)
                    (silver-brain-concept-summary-uuid concept-summary))
      (silver-brain--widget-insert-with-face (silver-brain-concept-summary-name concept-summary)
                                 '(:underline t))
    (let ((uuid (silver-brain-concept-summary-uuid concept-summary))
          (name (silver-brain-concept-summary-name concept-summary)))
      (silver-brain--with-concept-hyperlink-face
       (widget-create 'push-button
                      :notify (lambda (&rest _) (silver-brain-concept-open uuid))
                      (if (string-empty-p name) " " name))))))

(defun silver-brain--concept-rename (new-name)
  (let ((concept silver-brain-current-concept))
    (silver-brain--api-send-request
     (concat "concepts/" (silver-brain-concept-uuid concept))
     :method :patch
     :data `((:name . ,new-name)))
    (setf (silver-brain-concept-name silver-brain-current-concept) new-name)
    (rename-buffer (silver-brain--concept-get-buffer-name concept)))
  (run-hooks 'silver-brain-after-update-concept-hook))

(defun silver-brain--concept-update-content-type (content-type)
  (silver-brain-api-update-concept (silver-brain-concept-uuid silver-brain-current-concept)
                       :content-type content-type)
  (silver-brain-concept-refresh))

(defun silver-brain-delete-this-concept ()
  (interactive)
  (when (y-or-n-p "I will delete this concept and all the related links. Continue?")
    (let ((uuid (silver-brain-concept-uuid silver-brain-current-concept)))
      (kill-buffer)
      (silver-brain-delete-concept uuid)
      (run-hooks 'silver-brain-after-delete-concept-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Content Buffer                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-open-content ()
  (interactive)
  (let ((concept silver-brain-current-concept))
    (with-current-buffer (get-buffer-create
                          (format silver-brain-concept-content-buffer-name-format
                                  (silver-brain-concept-name concept)))
      (insert (silver-brain-concept-content concept))

      ;; Decide major mode.
      (funcall (cdr (assoc (silver-brain-concept-content-type concept)
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
  (silver-brain-api-update-concept (silver-brain-concept-uuid silver-brain-current-concept)
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
