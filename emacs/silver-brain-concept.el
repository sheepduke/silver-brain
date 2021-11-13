;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-common)
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
    (define-key map (kbd "g") 'silver-brain-concept-refresh)
    map))

(define-derived-mode silver-brain-concept-mode fundamental-mode "SB-Concept"
  "Major mode for Silver Brain single concept.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Buffer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-show (uuid)
  (let ((buffer (silver-brain--concept-prepare-buffer uuid)))
    (pop-to-buffer-same-window buffer)))

(defun silver-brain-concept-refresh ()
  (interactive)
  (with-current-buffer (silver-brain--concept-get-buffer-name silver-brain-current-concept)
    (silver-brain--concept-prepare-buffer (alist-get :uuid silver-brain-current-concept))))

(defun silver-brain--concept-get-buffer-name (concept)
  (format silver-brain-concept-buffer-name-format (alist-get :name concept)))

(defun silver-brain--concept-prepare-buffer (uuid)
  (with-current-buffer (silver-brain--api-send-request (concat "concept/" uuid))
    (let ((concept (silver-brain--api-read-json)))
      (silver-brain--with-widget-buffer (silver-brain--concept-get-buffer-name concept)
        (silver-brain-concept-mode)
        (setq silver-brain-current-concept concept)
        (silver-brain--concept-insert-widgets concept))
      (get-buffer (silver-brain--concept-get-buffer-name concept)))))

(defun silver-brain--concept-insert-widgets (concept)
  (silver-brain--widget-insert-with-face "Concept" 'silver-brain-concept-subtitle)
  
  (widget-insert "\n\n  Name: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 6)
                 :value (alist-get :name concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--concept-rename (widget-value widget))))
  (widget-insert "\n  Content Type: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 14)
                 :value (alist-get :content-type concept)
                 :action (lambda (widget &rest _)
                           (silver-brain--concept-update-content-type (widget-value widget))))
  (widget-insert "\n  Create Time: ")
  (widget-insert (silver-brain--time-to-string (alist-get :created-at concept)))
  (widget-insert "\n  Update Time: ")
  (widget-insert (silver-brain--time-to-string (alist-get :updated-at concept)))
  (widget-insert "\n\n  ")

  ;; Insert buttons.
  (silver-brain--with-push-button-face
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (silver-brain-concept-show (silver-brain-new-concept)))
                  "New")
   (widget-insert " ")
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (when (y-or-n-p "I will delete this concept and all the related links. Continue?")
                                 (silver-brain-delete-concept)
                                 (kill-buffer)))
                  "Delete")
   (widget-insert " ")
   (widget-create 'push-button
                  :notify (lambda (&rest _)
                            (kill-buffer))
                  "Back"))

  ;; Insert links. 
  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Inbound Links" 'silver-brain-concept-subtitle)
  (widget-insert "\n")

  (let ((links (silver-brain--concept-get-links "target")))
    (mapc (lambda (link)
            (widget-insert "\n  ")
            (silver-brain--with-push-button-face
             (widget-create 'push-button
                            :notify (lambda (&rest _))
                            "Unlink"))
            (widget-insert " ")
            (silver-brain--concept-insert-concept-button (alist-get :source link))
            (widget-insert " → ")
            (silver-brain--concept-insert-concept-button (alist-get :relation link))
            (widget-insert " → ")
            (silver-brain--widget-insert-with-face (alist-get :name (alist-get :target link))
                                       '(:underline t)))
          links))

  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Outbound Links" 'silver-brain-concept-subtitle)
  (widget-insert "\n")

  (let ((links (silver-brain--concept-get-links "source")))
    (mapc (lambda (link)
            (widget-insert "\n  ")
            (silver-brain--with-push-button-face
             (widget-create 'push-button
                            :notify (lambda (&rest _))
                            "Unlink"))
            (widget-insert " ")
            (silver-brain--widget-insert-with-face (alist-get :name (alist-get :source link))
                                       '(:underline t))
            (widget-insert " → ")
            (silver-brain--concept-insert-concept-button (alist-get :relation link))
            (widget-insert " → ")
            (silver-brain--concept-insert-concept-button (alist-get :target link)))
          links))
  
  ;; Insert content.
  (widget-insert "\n\n")
  (silver-brain--widget-insert-with-face "Content" 'silver-brain-concept-subtitle)
  (widget-insert "\n\n")
  (widget-insert (alist-get :content concept)))

(defun silver-brain--concept-insert-concept-button (concept-summary)
  (let ((uuid (alist-get :uuid concept-summary))
        (name (alist-get :name concept-summary)))
    (silver-brain--with-concept-hyperlink-face
     (widget-create 'push-button
                    :notify (lambda (&rest _) (silver-brain-concept-show uuid))
                    (if (string-empty-p name) " " name)))))

(defun silver-brain--concept-rename (new-name)
  (let ((concept silver-brain-current-concept))
    (silver-brain--api-send-request
     (concat "concept/" (alist-get :uuid concept))
     :method :patch
     :data (json-encode `((:name . ,new-name))))
    (silver-brain--alist-set :name silver-brain-current-concept new-name)
    (rename-buffer (silver-brain--concept-get-buffer-name concept)))
  (run-hooks 'silver-brain-after-concept-update-hook))

(defun silver-brain--concept-update-content-type (content-type)
  (silver-brain--api-send-request
   (concat "concept/" (alist-get :uuid silver-brain-current-concept))
   :method :patch
   :data (json-encode `((:content-type . ,content-type))))
  (silver-brain--alist-set :content-type silver-brain-current-concept content-type)
  (run-hooks 'silver-brain-after-concept-update-hook))

(defun silver-brain--concept-get-links (type)
  (let ((concept silver-brain-current-concept))
    (with-current-buffer (silver-brain--api-send-request
                          (format "concept-link?%s=%s"
                                  type
                                  (alist-get :uuid concept)))
      (silver-brain--api-read-json))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Content Buffer                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-open-content ()
  (interactive)
  (let ((concept silver-brain-current-concept))
    (with-current-buffer (get-buffer-create
                          (format silver-brain-concept-content-buffer-name-format
                                  (alist-get :name concept)))
      (insert (alist-get :content concept))

      ;; Decide major mode.
      (funcall (cdr (assoc (alist-get :content-type concept)
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
  (let ((concept silver-brain-current-concept)
        (new-content (buffer-string)))
    (with-current-buffer (silver-brain--api-send-request
                          (concat "concept/" (alist-get :uuid concept))
                          :method :patch
                          :data (json-encode `((:content . ,new-content)))))
    (setf (cdr (assoc :content silver-brain-current-concept)) new-content)
    (run-hooks 'silver-brain-after-concept-update-hook))

  (set-buffer-modified-p nil))

(defun silver-brain-concept-refresh-all-buffers ()
  "Refresh all the Silver Brain Concept buffers."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (silver-brain-concept-refresh)))
        (silver-brain-concept-get-all-buffers)))

(defun silver-brain-concept-get-all-buffers ()
  "Return all the Silver Brain Concept buffers."
  (remove-if-not (lambda (buffer)
                   (with-current-buffer buffer
                     (and (string-prefix-p "*Silver Brain Concept" (buffer-name))
                          (equal 'silver-brain-concept-mode major-mode))))
                 (buffer-list)))

(defun silver-brain--concept-install ()
  "Setup hooks etc for Silver Brain Concept buffers."
  (add-hook 'silver-brain-after-concept-update-hook 'silver-brain-concept-refresh-all-buffers)
  (add-hook 'after-change-major-mode-hook 'silver-brain-concept-setup-local-key))

(provide 'silver-brain-concept)
