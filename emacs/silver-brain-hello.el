;; Local Variables:
;; lexical-binding: t
;; nameless-current-name: "silver-brain-hello"
;; End:

(require 'widget)
(require 'wid-edit)

(require 'silver-brain-api)
(require 'silver-brain-common)

(defvar silver-brain-hello-buffer-name "*Silver Brain Hello*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode silver-brain-hello-mode fundamental-mode "SB-Hello"
  "Major mode for Silver Brain software.")

(defvar silver-brain-hello-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Function                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-hello ()
  "Show silver-brain-hello buffer."
  (silver-brain-hello--prepare-buffer)
  (pop-to-buffer-same-window (get-buffer silver-brain-hello-buffer-name)))

(defun silver-brain-hello--prepare-buffer ()
  "Prepare the silver-brain-hello buffer."
  (silver-brain--with-widget-buffer
   silver-brain-hello-buffer-name
   (silver-brain-hello-mode)
   (silver-brain-hello--insert-widgets))

  (with-current-buffer (get-buffer silver-brain-hello-buffer-name)
    (widget-forward 1)))

(defun silver-brain-hello--insert-widgets ()
  (widget-insert "Hello!
I am Silver, your personal external brain.\n\n")

  (widget-insert "Search: ")
  (widget-create 'editable-field
                 :size (silver-brain--get-textfield-length 8)
                 :action (lambda (widget &rest _event)
                           (silver-brain-list-show (widget-value widget))))
  
  (widget-insert "\n")
  (widget-insert "\nInput keywords separated by space to search."))

(provide 'silver-brain-hello)

;;; silver-brain-hello.el ends here
