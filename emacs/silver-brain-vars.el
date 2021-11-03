;;; silver-brain-vars.el -*- lexical-binding: t -*-

(defgroup silver-brain nil
  "Silver Brain customizations.")

(defcustom silver-brain-server-port 5001
  "The port of Silver Brain server."
  :type 'integer
  :group 'silver-brain)

(defcustom silver-brain-time-string "%Y-%m-%d %H:%M:%S"
  "The format string of time display."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-content-mode-alist
  '(("text/org" . org-mode)
    ("text/markdown" . markdown-mode)
    ("text/md" . markdown-mode)
    ("" . fundamental-mode))
  "The alist of mapping between concept's content type and major mode used to open it."
  :type 'alist
  :group 'silver-brain)

(provide 'silver-brain-vars)
