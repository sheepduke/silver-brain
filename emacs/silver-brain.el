;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-hello)

(defun silver-brain ()
  (interactive)
  (silver-brain-hello))

(defun silver-brain-install ()
  (silver-brain--list-install)
  (silver-brain--concept-install))

(provide 'silver-brain)

;;; silver-brain.el ends here
