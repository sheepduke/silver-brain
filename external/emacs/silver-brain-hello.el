;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-util)
(require 'silver-brain-item)

(defvar silver-brain-hello-buffer-name "*Silver Brain Hello*")

;; ============================================================
;;  Mode Definition
;; ============================================================

(defvar silver-brain-hello-mode-map
  (let ((keymap (make-composed-keymap (list (make-sparse-keymap)
                                            widget-keymap))))
    (set-keymap-parent keymap silver-brain-common-keymap)
    (define-key keymap (kbd "g") 'silver-brain-hello-refresh)
    (define-key keymap (kbd "G") 'silver-brain-hello-clear)
    (define-key keymap (kbd "m") 'silver-brain-mark-item-and-forward)
    (define-key keymap (kbd "u") 'silver-brain-unmark-item-and-forward)
    (define-key keymap (kbd "x d") 'silver-brain-delete-marked-items)
    keymap))

(define-derived-mode silver-brain-hello-mode fundamental-mode "SB-Hello"
  "Major mode for Silver Brain software.")

;; ============================================================
;;  Local Variable
;; ============================================================

(defvar silver-brain-hello-search-string nil
  "The current search string for Silver Brain Hello buffer.")

;; ============================================================
;;  Functions
;; ============================================================

(defun silver-brain-hello ()
  "Show hello buffer."
  (interactive)
  (silver-brain--hello-prepare-buffer)
  (silver-brain--hello-show))

(defun silver-brain--hello-prepare-buffer ()
  (silver-brain--with-widget-buffer silver-brain-hello-buffer-name (silver-brain-hello-mode)
    (widget-insert "Hello! I am Silver, your personal external brain.\n"
                   "Now only single word search is supported. :-)"
                   "\n\n")

    (widget-insert "Search: ")
    (widget-create 'editable-field
                   :size (silver-brain--get-textfield-length 8)
                   :action (lambda (widget &rest _event)
                             (let ((point (point)))
                               (setq silver-brain-hello-search-string (widget-value widget))
                               (silver-brain--hello-prepare-buffer)
                               (goto-char point)))
                   (or silver-brain-hello-search-string ""))
    (widget-insert "\n")
    
    ;; Insert New button.
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (silver-brain-create-item))
                   "New")
    (widget-insert "\n\n")

    ;; Insert search results.
    (when silver-brain-hello-search-string
      (let* ((items (silver-brain--item-get-sorted-items
                     (silver-brain-client-search-items silver-brain-hello-search-string))))

        ;; Insert the result summary line.
        (widget-insert (format "Results (%d):\n" (length items)))

        ;; Insert items.
        (dolist (item items)
          (widget-insert "  ")
          (silver-brain--widget-create-item item)
          (widget-insert "\n"))))))

(defun silver-brain--hello-show ()
  (let ((buffer (get-buffer silver-brain-hello-buffer-name)))
    (with-current-buffer buffer
      (widget-forward 1))
    (pop-to-buffer-same-window buffer)))

;; ============================================================
;;  Refresh & Clear
;; ============================================================

(defun silver-brain-hello-refresh ()
  (interactive)
  (silver-brain--hello-prepare-buffer))

(defun silver-brain-hello-clear ()
  (interactive)
  (setq silver-brain-hello-search-string nil)
  (silver-brain--hello-prepare-buffer))

;; ============================================================
;;  Mark
;; ============================================================

(defvar-local silver-brain--marked-items '())

(defun silver-brain-mark-item-and-forward ()
  (interactive)
  (silver-brain-mark-item)
  (silver-brain-forward-item))

(defun silver-brain-unmark-item-and-forward ()
  (interactive)
  (silver-brain-unmark-item)
  (silver-brain-forward-item))

(defun silver-brain-mark-item ()
  (interactive)
  (if (silver-brain--widget-has-item-in-line)
      (let ((inhibit-read-only t))
        (save-excursion
          (move-beginning-of-line 1)
          (delete-char 1)
          (insert ">")
          (push (silver-brain--widget-get-item-in-line) silver-brain--marked-items)
          (set-buffer-modified-p nil)))
    (message "Must be invoked at a line with item")))

(defun silver-brain-unmark-item ()
  (interactive)
  (if (silver-brain--widget-has-item-in-line)
      (let ((inhibit-read-only t))
        (save-excursion
          (move-beginning-of-line 1)
          (delete-char 1)
          (insert " ")
          (setq silver-brain--marked-items
                (delete (silver-brain--widget-get-item-in-line) silver-brain--marked-items))
          (set-buffer-modified-p nil)))
    (message "Must be invoked at a line with item")))

(defun silver-brain-hello-delete-marked-items ()
  (interactive)
  (silver-brain--delete-items (seq-map #'silver-brain--prop-id silver-brain--marked-items)))

(provide 'silver-brain-hello)
