;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-common)
(require 'silver-brain-list)

(defvar silver-brain-hello-buffer-name "*Silver Brain Hello*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-hello-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    map))

(define-derived-mode silver-brain-hello-mode fundamental-mode "SB-Hello"
  "Major mode for Silver Brain software.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Function                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-hello ()
  "Show hello buffer."
  ;; Prepare the hello buffer.
  (silver-brain--with-widget-buffer silver-brain-hello-buffer-name (silver-brain-hello-mode)
    (widget-insert "Hello!
I am Silver, your personal external brain.\n\n")

    (widget-insert "Search: ")
    (widget-create 'editable-field
                   :size (silver-brain--get-textfield-length 8)
                   :action (lambda (widget &rest _event)
                             (silver-brain-list-show (widget-value widget))))
    (widget-insert "\n")
    
    ;; Insert New button.
    (widget-insert "\n")
    (silver-brain--with-push-button-face
     (widget-create 'push-button
                    :notify (lambda (&rest _) (silver-brain-create-concept))
                    "New"))
    (widget-insert "\n")
    
    (widget-insert "\nInput keywords separated by space to search."))

  ;; Switch to the hello buffer.
  (let ((buffer (get-buffer silver-brain-hello-buffer-name)))
    (with-current-buffer buffer
      (widget-forward 1))
    (pop-to-buffer-same-window buffer)))

(provide 'silver-brain-hello)
