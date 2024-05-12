;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-hello)

(defun silver-brain ()
  (interactive)
  (silver-brain-hello))

(defun silver-brain-open ()
  "Search and open concept."
  (interactive)
  (silver-brain-item-open (silver-brain--search-items-and-select
               (read-string "Search items: "))))

(provide 'silver-brain)

;;; silver-brain.el ends here
