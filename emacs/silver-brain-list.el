;;; silver-brain-list.el -*- lexical-binding: t -*-

(require 'widget)
(require 'wid-edit)
(require 'silver-brain-common)
(require 'silver-brain-api)

(defvar silver-brain-list-buffer-name "*silver-brain-list*")

(defvar-local silver-brain-list--search-string nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Mode                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-list-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "g") 'silver-brain-list-refresh)
    map))

(define-derived-mode silver-brain-list-mode fundamental-mode "SB-List"
  "Major mode for Silver Brain list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Function                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-list-show (search-string)
  (with-current-buffer (silver-brain-api-send-request
                        (format "concept?search=%s" search-string))
    (silver-brain-list--prepare-buffer (silver-brain-api-read-json)))
  (with-current-buffer (get-buffer silver-brain-list-buffer-name)
    (setq silver-brain-list--search-string search-string)
    (put 'silver-brain-list--search-string 'permanent-local t)
    (pop-to-buffer-same-window (current-buffer))))

(defun silver-brain-list-refresh ()
  (interactive)
  (silver-brain-list-show silver-brain-list--search-string))

(defun silver-brain-list--prepare-buffer (concept-list)
  (silver-brain--with-widget-buffer
   silver-brain-list-buffer-name
   (silver-brain-list-mode)
   (if (null concept-list)
       (silver-brain-list--insert-not-found)
     (silver-brain-list--insert-buttons concept-list))))

(defun silver-brain-list--insert-not-found ()
  (widget-insert "No concept found."))

(defun silver-brain-list--insert-buttons (concept-list)
  (widget-insert (format "%d concepts found.\n"
                         (length concept-list)))
  (mapc (lambda (concept)
          (widget-create 'push-button
                         :notify (lambda (&rest _)
                                   (silver-brain-concept-show
                                    (alist-get :uuid concept)))
                         (alist-get :name concept))
          (widget-insert "\n"))
        (sort concept-list
              (lambda (s1 s2)
                (string< (alist-get :name s2)
                         (alist-get :name s1))))))

(provide 'silver-brain-list)
