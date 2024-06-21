;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-
;;; 
(require 'tablist)
(require 'seq)
(require 'major-mode-hydra)

(require 'silver-brain-client)
(require 'silver-brain-item)

(defvar silver-brain-list-buffer-name "*SB/List*")

(defvar-local silver-brain-list-items nil
  "The list of items as search results.")

(defvar-local silver-brain-list-search-string nil
  "The search string.")

(define-derived-mode silver-brain-list-mode tablist-mode "SB/List"
  (setq tabulated-list-format
        [("ID" 27 t)
         ("Name" 40 t)])
  (tabulated-list-init-header)

  (setq tablist-operations-function #'silver-brain--list-operations)

  (define-key silver-brain-list-mode-map (kbd "<SPC>") #'major-mode-hydra)
  (define-key silver-brain-list-mode-map (kbd "g") #'tablist-revert)
  (define-key silver-brain-list-mode-map (kbd "G") #'silver-brain-list-refresh))

(major-mode-hydra-define silver-brain-list-mode ()
  ("Buffer"
   (("g" silver-brain-list-refresh "refresh")
    ("q" tablist-quit "quit"))
   
   "Row"
   (("s" tablist-sort "sort")
    ("k" tablist-do-kill-lines "hide"))

   "Column"
   (("<" tablist-shrink-column "shrink column")
    (">" tablist-enlarge-column "enlarge column"))

   "Mark"
   (("m" tablist-mark-forward "mark")
    ("u" tablist-unmark-forward "unmark")
    ("U" tablist-unmark-all-marks "unmark all")
    ("t" tablist-toggle-marks "toggle")
    ("d" tablist-flag-forward "mark as delete"))

   "Manipulation"
   (("D" tablist-do-delete "delete")
    ("x" tablist-do-flagged-delete "delete flagged"))))

(defun silver-brain-list-items (&optional search-string)
  "Search items with given search string and display them in a buffer."
  (interactive "sSearch string: ")
  (setq silver-brain-list-search-string search-string)
  (switch-to-buffer (get-buffer-create silver-brain-list-buffer-name))
  (silver-brain-list-mode)

  (setq silver-brain-list-items (silver-brain-client-search-items search-string))
  (setq tabulated-list-entries (silver-brain--list-items-to-tablist))

  (tabulated-list-print))

(defun silver-brain-list-refresh ()
  (interactive)
  (silver-brain-list-items silver-brain-list-search-string))

;; ============================================================
;;  Internal
;; ============================================================

(cl-defun silver-brain--list-items-to-tablist (&optional (items silver-brain-list-items))
  (seq-map (lambda (item)
             (list (silver-brain--prop-id item)
                   (vector (silver-brain--prop-id item)
                           (silver-brain--prop-name item))))
           items))

(defun silver-brain--list-operations (&rest args)
  (cl-case (cl-first args)
    (supported-operations '(delete find-entry edit-column complete))
    (find-entry (silver-brain-item-open (cl-second args)))
    (delete (silver-brain--list-delete (cl-second args)))))

(defun silver-brain--list-delete (ids)
  (dolist (id ids)
    (silver-brain-client-delete-item id)
    (setq silver-brain-list-items
          (seq-remove (lambda (item)
                        (string-equal (silver-brain--prop-id item) id))
                      silver-brain-list-items)))

  (setq tabulated-list-entries (silver-brain--list-items-to-tablist)))

(provide 'silver-brain-list)
