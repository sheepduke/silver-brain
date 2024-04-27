;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-hello)

(defun silver-brain ()
  (interactive)
  (silver-brain-hello))

(defun silver-brain-open ()
  "Search and open concept."
  (interactive)
  (silver-brain-concept-open (silver-brain--search-items-and-select
                  (read-string "Search items: "))))

(defun silver-brain-install ()
  (silver-brain--list-install)
  (silver-brain--item-install)
  nil)

(provide 'silver-brain)

;;; silver-brain.el ends here
