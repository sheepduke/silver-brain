;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'silver-brain-util)
(require 'silver-brain-item)

(defvar silver-brain-list-buffer-name "*Silver Brain List*")

(defvar-local silver-brain-list-search-string nil
  "The current search string for Silver Brain List buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-list-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "g") 'silver-brain-list-refresh)
    map)
  "The keymap used for Silver Brain List mode.")

(define-derived-mode silver-brain-list-mode fundamental-mode "SB/List"
  "Major mode for Silver Brain List.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Function                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-list-show (search-string)
  "Show Silver Brain List buffer."
  (silver-brain--list-prepare-buffer search-string)
  (pop-to-buffer-same-window (get-buffer silver-brain-list-buffer-name)))

(defun silver-brain-list-refresh ()
  "Refresh Silver Brain List buffer."
  (interactive)
  (if-let ((buffer (get-buffer silver-brain-list-buffer-name)))
      (with-current-buffer buffer
        (silver-brain--list-prepare-buffer silver-brain-list-search-string))))

(defun silver-brain--list-prepare-buffer (search-string)
  "Prepare the Silver Brain List buffer. The data is fetched
using given SEARCH-STRING."
  (let ((items (thread-first (silver-brain-client-search-items search-string)
                             (sort (lambda (x y)
                                     (string< (silver-brain--prop-id x)
                                              (silver-brain--prop-id y))))
                             (sort (lambda (x y)
                                     (string< (silver-brain--prop-name x)
                                              (silver-brain--prop-name y)))))))
    (silver-brain--with-widget-buffer silver-brain-list-buffer-name
      (silver-brain-list-mode)
      (setq silver-brain-list-search-string search-string)
      (setq silver-brain-refresh-function 'silver-brain-list-refresh)
      (silver-brain--list-create-widgets items))
    (with-current-buffer silver-brain-list-buffer-name
      (goto-char (point-min))
      (ignore-errors (widget-forward 1)))))

(defun silver-brain--list-create-widgets (items)
  "Create inserts to "
  (let ((item-count (length items)))
    (if (= 0 item-count)
        (widget-insert "I dit not find any item. :-(")
      (widget-insert (format "I found %d councepts. :-)\n\n" item-count))))

  ;; Insert item buttons.
  (mapc (lambda (item)
          (silver-brain--with-item-hyperlink-face
           (widget-create 'push-button
                          :notify (lambda (&rest _)
                                    (silver-brain-item-open
                                     (silver-brain--prop-id item)))
                          (silver-brain--prop-name item)))
          (widget-insert "\n"))
        items))

(defun silver-brain-list-on-item-changed (before after)
  (silver-brain-list-refresh))

(defun silver-brain--list-install ()
  "Install hooks etc."
  (add-hook 'silver-brain-after-create-item-hook 'silver-brain-list-on-item-changed)
  (add-hook 'silver-brain-after-update-item-hook 'silver-brain-list-on-item-changed)
  (add-hook 'silver-brain-after-delete-item-hook 'silver-brain-list-on-item-changed))

(provide 'silver-brain-list)
