;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defgroup silver-brain nil
  "Silver Brain customizations."
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Customs                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom silver-brain-server-port 5001
  "The port of Silver Brain server."
  :type 'integer
  :group 'silver-brain)

(defcustom silver-brain-time-format "%Y-%m-%d %H:%M:%S"
  "The format string of time display."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-content-mode-alist '(("text/org" . org-mode)
                                 ("text/markdown" . markdown-mode)
                                 ("text/md" . markdown-mode)
                                 ("" . fundamental-mode))
  "The alist of mapping between concept's content type and major
mode used to open it."
  :type 'alist
  :group 'silver-brain)

(defcustom silver-brain-database-name nil
  "The default database name."
  :type 'string
  :group 'silver-brain)

(defcustom silver-brain-default-content-type "text/org"
  "The default content type for concept."
  :type 'string
  :group 'silver-brain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Hooks                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar silver-brain-after-create-concept-hook '())

(defvar silver-brain-after-update-concept-hook '())

(defvar silver-brain-after-delete-concept-hook '())

(defvar-local silver-brain-refresh-function nil)
(put 'silver-brain-refresh-function 'permanent-local t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Faces                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface silver-brain-concept-hyperlink '((((class color)
                                (background dark))
                               :foreground "LightBlue1"
                               :underline t)
                              (((class color)
                                (background light))
                               :foreground "blue"
                               :underline t))
  "Face used for concept hyperlinks."
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

(defface silver-brain-concept-subtitle '((t :underline t
                                :height 1.5
                                :weight bold))
  "Face used for subtitle in concept buffer."
  :group 'silver-brain)

(provide 'silver-brain-vars)
