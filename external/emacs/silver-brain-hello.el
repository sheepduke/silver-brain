;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-util)

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
                             (setq silver-brain-hello-search-string (widget-value widget))
                             (silver-brain--hello-prepare-buffer))
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
      (let* ((items (thread-first (silver-brain-client-search-items silver-brain-hello-search-string)
                                  (sort (lambda (x y)
                                          (string< (silver-brain--prop-id x)
                                                   (silver-brain--prop-id y))))
                                  (sort (lambda (x y)
                                          (string< (silver-brain--prop-name x)
                                                   (silver-brain--prop-name y)))))))

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
;;  Commands
;; ============================================================

(defun silver-brain-hello-refresh ()
  (interactive)
  (silver-brain--hello-prepare-buffer))

(defun silver-brain-hello-clear ()
  (interactive)
  (setq silver-brain-hello-search-string nil)
  (silver-brain--hello-prepare-buffer))

(provide 'silver-brain-hello)
