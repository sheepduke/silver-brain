;;; silver-brain-concept.el -*- lexical-binding: t -*-

(require 'widget)
(require 'wid-edit)
(require 'silver-brain-common)
(require 'silver-brain-api)
(require 'silver-brain-vars)

(defvar silver-brain-concept-buffer-name-format "*silver-brain-concept %s*")

(defvar silver-brain-concept-content-buffer-name-format "*silver-brain-concept-content %s*")

(defvar-local silver-brain-current-concept nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-concept-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "e") 'silver-brain-concept-open-content)
    (define-key map (kbd "g") 'silver-brain-concept--refresh)
    map))

(define-derived-mode silver-brain-concept-mode fundamental-mode "SB-Concept"
  "Major mode for Silver Brain single concept.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Buffer                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-concept-show (uuid)
  (let ((buffer (silver-brain-concept--prepare-buffer uuid)))
    (pop-to-buffer-same-window buffer)))

(defun silver-brain-concept--refresh ()
  (interactive)
  (with-current-buffer (silver-brain-concept--get-buffer-name silver-brain-current-concept)
    (silver-brain-concept--prepare-buffer (alist-get :uuid silver-brain-current-concept))))

(defun silver-brain-concept--get-buffer-name (concept)
  (format silver-brain-concept-buffer-name-format (alist-get :name concept)))

(defun silver-brain-concept--prepare-buffer (uuid)
  (with-current-buffer (silver-brain-api-send-request (concat "concept/" uuid))
    (let ((concept (silver-brain-api-read-json)))
      (silver-brain--with-widget-buffer
       (silver-brain-concept--get-buffer-name concept)
       (silver-brain-concept-mode)
       (silver-brain-concept--insert-widgets concept)
       (setq silver-brain-current-concept concept))
      (get-buffer (silver-brain-concept--get-buffer-name concept)))))

(defun silver-brain-concept--insert-widgets (concept)
  (widget-insert "Name: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 6)
                 :value (alist-get :name concept)
                 :action (lambda (widget &rest _)
                           (silver-brain-concept--rename (widget-value widget))))
  (widget-insert "\nContent Type: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 14)
                 :value (alist-get :content-type concept)
                 :action (lambda (widget &rest _)
                           (silver-brain-concept--update-content-type (widget-value widget))))
  (widget-insert "\n")
  (widget-insert "Create Time: ")
  (widget-insert (silver-brain--display-time (alist-get :created-at concept)))
  (widget-insert "\n")
  (widget-insert "Update Time: ")
  (widget-insert (silver-brain--display-time (alist-get :updated-at concept)))
  (widget-insert "\n\n")

  ;; Insert buttons.
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain-concept-show (silver-brain-concept-create)))
                 "Create")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (silver-brain-concept-delete)
                           (quit-window))
                 "Delete")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (kill-buffer))
                 "Back")

  ;; Insert content.
  (widget-insert "\n\n")
  (widget-insert (make-string (window-width) ?-))
  (widget-insert "\n\n")
  (widget-insert (alist-get :content concept)))

(defun silver-brain-concept--rename (new-name)
  (let ((concept silver-brain-current-concept))
    (silver-brain-api-send-request
     (concat "concept/" (alist-get :uuid concept))
     :method :patch
     :data (json-encode `((:name . ,new-name))))
    (alist-set :name silver-brain-current-concept new-name)
    (rename-buffer (silver-brain-concept--get-buffer-name concept)))
  (run-hooks 'silver-brain-after-concept-update-hook))

(defun silver-brain-concept--update-content-type (content-type)
  (silver-brain-api-send-request
   (concat "concept/" (alist-get :uuid silver-brain-current-concept))
   :method :patch
   :data (json-encode `((:content-type . ,content-type))))
  (alist-set :content-type silver-brain-current-concept content-type)
  (run-hooks 'silver-brain-after-concept-update-hook))

(defun silver-brain-concept-create ()
  (let* ((name (read-string "Concept name: "))
         uuid)
    (with-current-buffer (silver-brain-api-send-request
                          "concept"
                          :method :post
                          :data (json-encode `((:name . ,name))))
      (setq uuid (silver-brain-api-body-string)))
    (run-hooks 'silver-brain-after-concept-create-hook)
    uuid))

(defun silver-brain-concept-delete ()
  (let ((concept silver-brain-current-concept))
    (with-current-buffer (silver-brain-api-send-request
                          (concat "concept/" (alist-get :uuid concept))
                          :method :delete)))
  (run-hooks 'silver-brain-after-concept-delete-hook))

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
      (put 'silver-brain-current-concept 'permanent-local t)
      
      ;; Set local keys.
      (silver-brain-concept-setup-local-key)

      (set-buffer-modified-p nil)
      (pop-to-buffer-same-window (current-buffer)))))

(defun silver-brain-concept-setup-local-key ()
  (when (and (not (null silver-brain-current-concept)))
    (local-set-key (kbd "C-x C-s") 'silver-brain-concept-save-content)))

(defun silver-brain-concept-save-content ()
  (interactive)
  (let ((concept silver-brain-current-concept)
        (new-content (buffer-string)))
    (with-current-buffer (silver-brain-api-send-request
                          (concat "concept/" (alist-get :uuid concept))
                          :method :patch
                          :data (json-encode `((:content . ,new-content)))))
    (setf (cdr (assoc :content silver-brain-current-concept)) new-content)
    (run-hooks 'silver-brain-after-concept-update-hook))

  (set-buffer-modified-p nil))

(add-hook 'silver-brain-after-concept-update-hook 'silver-brain-concept--refresh)

(add-hook 'after-change-major-mode-hook 'silver-brain-concept-setup-local-key)

(provide 'silver-brain-concept)
