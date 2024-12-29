;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defgroup silver-brain nil
  "Silver Brain customizations."
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Customs                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom silver-brain-server-port 8080
  "The port of Silver Brain server."
  :type 'integer
  :group 'silver-brain)

(defcustom silver-brain-time-format "%Y-%m-%d %H:%M:%S"
  "The format string of time display."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-list-buffer-name "*SB/List*"
  "The name of list buffer."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-item-buffer-name "*SB/Item*"
  "The name of item buffer."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-item-content-buffer-name "*SB/Content*"
  "The name of item content buffer."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-content-mode-alist '(("text/org" . org-mode)
                                 ("application/org" . org-mode)
                                 ("text/markdown" . markdown-mode)
                                 (nil . fundamental-mode))
  "The alist of mapping between item's content type and major
mode used to open it."
  :type 'alist
  :group 'silver-brain)

(defcustom silver-brain-store-name "main"
  "The store name."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-default-content-type "text/org"
  "The default content type for item."
  :type 'string
  :group 'silver-brain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Faces                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface silver-brain-item-hyperlink '((((class color)
                             (background dark))
                            :foreground "LightBlue1"
                            :underline t)
                           (((class color)
                             (background light))
                            :foreground "blue"
                            :underline t))
  "Face used for item hyperlinks."
  :group 'silver-brain)

(defface silver-brain-push-button '((((class color)
                          (background dark))
                         :foreground "MediumPurple1"
                         :box t)
                        (((class color)
                          (background light))
                         :foreground "purple3"
                         :box t))
  "Face used for push buttons."
  :group 'silver-brain)

(defface silver-brain-h1 '((t :height 1.5
                  :weight bold
                  :underline t))
  "Face for level 1 header."
  :group 'silver-brain)

(defface silver-brain-h2 '((t :height 1.2
                  :weight bold))
  "Face for level 2 header"
  :group 'silver-brain)

(provide 'silver-brain-vars)
