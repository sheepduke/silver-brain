;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-hello)

(defun silver-brain ()
  (interactive)
  (silver-brain-hello))

(defun silver-brain-open ()
  "Search and open concept."
  (interactive)
  (silver-brain-concept-show (silver-brain--search-concept-and-select)))

(defun silver-brain-install ()
  (silver-brain--list-install)
  (silver-brain--concept-install)
  nil)

(provide 'silver-brain)

;;; silver-brain.el ends here
