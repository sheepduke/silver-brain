;;; Package --- silver-brain-list

;;; Commentary:
;;;
;;; List concepts.

;;; Code:

(require 'silver-brain-util)

(defvar silver-brain-list-buffer-name "*Silver Brain - List*"
  "Name of buffer used to list concepts.")

(defvar-local silver-brain-list--query ""
  "Query string of current search.")

(define-derived-mode silver-brain-list-mode special-mode "Brain-Concepts"
  "Major mode for listing result of concepts."
  )

(define-key silver-brain-list-mode-map (kbd "<return>") 'silver-brain-follow-link)
(define-key silver-brain-list-mode-map (kbd "<tab>") 'silver-brain-jump-to-next-link)
(define-key silver-brain-list-mode-map (kbd "<backtab>") 'silver-brain-jump-to-previous-link)
(define-key silver-brain-list-mode-map (kbd "s") 'silver-brain-list)
(define-key silver-brain-list-mode-map (kbd "g") 'silver-brain-list-refresh)
(define-key silver-brain-list-mode-map (kbd "p") 'previous-line)
(define-key silver-brain-list-mode-map (kbd "n") 'next-line)

(defun silver-brain-list ()
  "List concepts by searching."
  (interactive)
  (let* ((query (read-string "Search: "))
         (concepts (silver-brain-api--search-concept query)))
    (silver-brain-list--setup-buffer concepts)))

(defun silver-brain-list--setup-buffer (concepts)
  "Setup buffer for search result CONCEPTS."
  (when (get-buffer silver-brain-list-buffer-name)
    (kill-buffer silver-brain-list-buffer-name))
  (switch-to-buffer silver-brain-list-buffer-name)
  (dolist (concept concepts)
    (silver-brain--insert-link concept)
    (insert "\n"))
  (when (not concepts)
    (insert "No concept defined.\n"))
  (silver-brain-list-mode)
  (goto-char (point-min)))

(provide 'silver-brain-list)
;;; silver-brain-list.el ends here
