;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)

(require 'silver-brain-vars)

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'silver-brain-refresh)
    (define-key map (kbd "j") 'silver-brain-widget-jump)
    (define-key map (kbd "n") 'silver-brain-new-concept)
    (define-key map (kbd "s") 'silver-brain-open)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "Q") 'silver-brain-quit-all)
    (define-key map [remap self-insert-command] 'silver-brain--no-edit)
    map)
  "Common keymap shared by all the Silver Brain buffers.")

(defun silver-brain--no-edit ()
  "Refuse to allow editing of Custom buffer."
  (interactive)
  (error "Undefined key binding"))

(defun silver-brain-refresh ()
  (interactive)
  (funcall silver-brain-refresh-function))

(defun silver-brain-quit-all ()
  "Kill all the Silver Brain buffers."
  (interactive)
  (mapc (lambda (buffer) (kill-buffer buffer))
        (cl-remove-if-not (lambda (buffer)
                            (string-prefix-p "*Silver Brain" (buffer-name buffer)))
                          (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Widget                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro silver-brain--with-widget-buffer (buffer-name &rest body)
  "Wrap basic buffer setup functions."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (let ((inhibit-read-only t))
       (mapc 'widget-delete widget-field-list)
       (erase-buffer)
       ,@body
       (widget-setup)
       (set-buffer-modified-p nil)
       (goto-char (point-min)))))

(defun silver-brain--get-textfield-length (length)
  "Return the width of text field widget. LENGTH is the extra
length to be removed."
  (max 8 (- (min 80 (window-width)) 10 length)))

(defun silver-brain--time-to-string (time)
  "Convert given TIME to string. TIME is a timestamp."
  (format-time-string silver-brain-time-format time))

(defun silver-brain--get-widgets ()
  "Get a list of points of widgets."
  (let (widget-points)
    (save-excursion
      (goto-char (point-min))
      (when (widget-at)
        (push (point) widget-points))
      (widget-forward 1)

      (while (and (widget-at)
                  (not (member (point) widget-points)))
        (push (point) widget-points)
        (widget-forward 1)))
    (nreverse widget-points)))

(defun silver-brain-widget-jump ()
  "Jump to the widgets."
  (interactive)
  (avy-process (mapcar (lambda (point)
                         (cons point (selected-window)))
                       (silver-brain--get-widgets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Buffer Style                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro silver-brain--with-concept-hyperlink-face (&rest body)
  `(let ((widget-button-face 'silver-brain-concept-hyperlink)
         (widget-push-button-prefix nil)
         (widget-push-button-suffix nil))
     ,@body))

(defmacro silver-brain--with-push-button-face (&rest body)
  `(let ((widget-button-face 'silver-brain-push-button)
         (widget-push-button-prefix " ")
         (widget-push-button-suffix " "))
     ,@body))

(defun silver-brain--widget-insert-with-face (text face)
  (let ((start (point)))
    (widget-insert text)
    (let ((end (point)))
      (add-face-text-property start end face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Api                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun silver-brain-new-concept (&optional name)
  "Create a new concept. If NAME is given, it is used as the name
of new concept. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Concept name: ")))
         (concept (silver-brain-api-create-concept name silver-brain-default-content-type)))
    (run-hooks 'silver-brain-after-concept-create-hook)
    (silver-brain-concept-show concept)))

(cl-defun silver-brain--search-concept-and-select (&optional (prompt "Search string: "))
  "Ask for a search string, search for concepts and select
one. PROMPT is the prompt for search string."
  (let* ((result (silver-brain--search-concept (read-string prompt)))
         (concepts (mapcar (lambda (alist) (cons (silver-brain-concept-summary-name alist)
                                                 (silver-brain-concept-summary-uuid alist)))
                           result)))
    (and concepts
         (let ((key (completing-read "Choose concept: " concepts)))
           (cdr (assoc-string key concepts))))))

(provide 'silver-brain-common)
