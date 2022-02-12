;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'silver-brain-common)
(require 'silver-brain-concept)

(defvar silver-brain-list-buffer-name "*Silver Brain List*")

(defvar-local silver-brain-list-search-string nil
  "The current search string for Silver Brain List buffer.")
(put 'silver-brain-list-search-string 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-list-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    map)
  "The keymap used for Silver Brain List mode.")

(define-derived-mode silver-brain-list-mode fundamental-mode "SB-List"
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
  (if-let ((buffer (get-buffer silver-brain-list-buffer-name)))
      (with-current-buffer buffer
        (silver-brain--list-prepare-buffer silver-brain-list-search-string))))

(defun silver-brain--list-prepare-buffer (search-string)
  "Prepare the Silver Brain List buffer. The data is fetched
using given SEARCH-STRING."
  (let ((concept-list (thread-first (silver-brain-api-search-concept search-string)
                        (sort #'silver-brain-concept-summary-by-uuid-<)
                        (sort #'silver-brain-concept-summary-by-name-<))))
    (silver-brain--with-widget-buffer silver-brain-list-buffer-name
      (silver-brain-list-mode)
      (setq silver-brain-list-search-string search-string)
      (setq silver-brain-refresh-function 'silver-brain-list-refresh)
      (silver-brain--list-create-widgets concept-list))
    (with-current-buffer silver-brain-list-buffer-name
      (goto-char (point-min))
      (ignore-errors (widget-forward 1)))))

(defun silver-brain--list-create-widgets (concept-list)
  "Create inserts to "
  (let ((concept-count (length concept-list)))
    (if (= 0 concept-count)
        (widget-insert "I dit not find any concept. :-(")
      (widget-insert (format "I found %d councepts. :-)\n\n" concept-count))))

  ;; Insert concept buttons.
  (mapc (lambda (concept)
          (silver-brain--with-concept-hyperlink-face
           (widget-create 'push-button
                          :notify (lambda (&rest _)
                                    (silver-brain-concept-open
                                     (silver-brain-concept-summary-uuid concept)))
                          (silver-brain-concept-summary-name concept)))
          (widget-insert "\n"))
        concept-list))

(defun silver-brain--list-install ()
  "Install hooks etc."
  (add-hook 'silver-brain-after-create-concept-hook 'silver-brain-list-refresh)
  (add-hook 'silver-brain-after-rename-concept-hook 'silver-brain-list-refresh)
  (add-hook 'silver-brain-after-delete-concept-hook 'silver-brain-list-refresh))

(provide 'silver-brain-list)
