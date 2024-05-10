;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)
(require 'silver-brain-item-content)

(defvar silver-brain-item-buffer-name-format "*Silver Brain Item - %s*")

(defvar silver-brain-item-content-buffer-name-format "*Silver Brain Item Content - %s*")

(defvar-local silver-brain-current-item nil)

;; ============================================================
;;  Mode
;; ============================================================

(defvar silver-brain-item-mode-map
  (let ((map (make-composed-keymap (list (make-sparse-keymap)
                                         widget-keymap))))
    (set-keymap-parent map silver-brain-common-keymap)
    (define-key map (kbd "g") 'silver-brain-item-refresh)
    (define-key map (kbd "e") 'silver-brain-item-open-content)
    (define-key map (kbd "d") 'silver-brain-delete-item-at-point)
    (define-key map (kbd "D") 'silver-brain-item-delete)
    (define-key map (kbd "r") 'silver-brain-item-rename)
    (define-key map (kbd "u t") 'silver-brain-item-update-content-type)
    map))

(define-derived-mode silver-brain-item-mode fundamental-mode "SB/Item"
  "Major mode for Silver Brain single item.")

;; ============================================================
;;  Buffer
;; ============================================================

(defun silver-brain-item-open (id)
  (silver-brain-item-show (silver-brain-client-get-item id)))

(defun silver-brain-item-show (item)
  (let ((buffer (silver-brain--item-prepare-buffer item)))
    (pop-to-buffer-same-window buffer)))

(defun silver-brain--item-prepare-buffer (item)
  (silver-brain--with-widget-buffer (silver-brain--item-get-buffer-name item)
    (silver-brain-item-mode)
    (setq silver-brain-current-item item)
    (enable-word-wrap)
    (silver-brain--item-insert-widgets item)))

(defun silver-brain--item-get-buffer-name (item)
  (format silver-brain-item-buffer-name-format (silver-brain--prop-name item)))

(defun silver-brain--item-insert-widgets (item)
  (silver-brain--widget-insert-with-face (format "%s "
                                     (silver-brain--prop-name silver-brain-current-item))
                             'silver-brain-h1)
  (silver-brain--widget-create-button "New" (lambda (&rest _) (silver-brain-create-item)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Rename" (lambda (&rest _) (silver-brain-item-rename)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Delete" (lambda (&rest _) (silver-brain-item-delete)))
  (widget-insert " ")
  (silver-brain--widget-create-button "Close" (lambda (&rest _) (kill-buffer)))

  (widget-insert "\n\n"
                 "  Content Type: "
                 (silver-brain--prop-content-type)
                 " ")

  (silver-brain--widget-create-button "Edit" (lambda (&rest _) (silver-brain-item-update-content-type)))

  (widget-insert "\n  Create Time: "
                 (silver-brain--format-time (silver-brain--prop-create-time item))
                 "\n  Update Time: "
                 (silver-brain--format-time (silver-brain--prop-update-time item))
                 "\n")
  
  ;; Insert parents.
  (widget-insert "\n")
  (silver-brain--item-insert-parents-or-children t)
  
  ;; Insert children.
  (widget-insert "\n")
  (silver-brain--item-insert-parents-or-children nil)

  ;; Insert references.
  (widget-insert "\n")
  (silver-brain--item-insert-references)

  ;; Insert content.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Content\n" 'silver-brain-h1)
  (widget-insert (make-horizontal-bar 60)
                 "\n"
                 (or (silver-brain--prop-content item) "")))

(cl-defun make-horizontal-bar (length)
  (string-join (cl-loop for i from 1 to length collect "⎯")))

(defun silver-brain--verify-current-item ()
  (unless silver-brain-current-item
    (error "This command must be invoked within a item buffer")))

(defun silver-brain--item-insert-parents-or-children (parentp)
  (silver-brain--widget-insert-with-face (if parentp "Parents " "Children ") 'silver-brain-h2)
  (silver-brain--widget-create-button
   "New" (lambda (&rest _)
           (let* ((this-id (silver-brain--prop-id))
                  (other-id (silver-brain--search-items-and-select
                             (read-string (format "Search for %s: "
                                                  (if parentp "parent" "child"))))))
             (if parentp
                 (silver-brain-client-create-child other-id this-id)
               (silver-brain-client-create-child this-id other-id))
             (silver-brain-item-refresh-when-id-in (list this-id other-id)))))

  (let ((others (silver-brain-client-get-items (if parentp
                                       (silver-brain--prop-parents)
                                     (silver-brain--prop-children)))))
    (unless (null others)
      (widget-insert "\n\n"))
    
    (widget-insert "  ")
    
    (dolist (other (silver-brain--item-get-sorted-items others))
      (silver-brain--widget-create-button
       "Del" (lambda (&rest _)
               (when (y-or-n-p "Confirm? ")
                 (let* ((this-id (silver-brain--prop-id))
                        (other-id (silver-brain--prop-id other)))
                   (if parentp
                       (silver-brain-client-delete-child other-id this-id)
                     (silver-brain-client-delete-child this-id other-id))
                   
                   (silver-brain-item-refresh-when-id-in (list this-id other-id))))))
      (widget-insert " ")
      (silver-brain--widget-create-item other)
      (widget-insert "\n  "))))

(defun silver-brain--item-insert-references ()
  (silver-brain--widget-insert-with-face "References " 'silver-brain-h2)
  (silver-brain--widget-create-button
   "New" (lambda ()
           (let* ((source (silver-brain--prop-id))
                  (target (silver-brain--search-items-and-select
                           (read-string "Search for target item: "))))
             (when target
               (let ((annotation (read-string "Annotation: ")))
                 (silver-brain-client-create-reference source target annotation)
                 (silver-brain-item-refresh-when-id-in (list source target)))))))
  (widget-insert "\n")

  (let* ((references-out (silver-brain-client-get-references (silver-brain--prop-references-out)))
         (references-in (silver-brain-client-get-references (silver-brain--prop-references-in)))
         (items-seq (silver-brain-client-get-items
                     (append (seq-map #'silver-brain--prop-target references-out)
                             (seq-map #'silver-brain--prop-source references-in))))
         (items (seq-map (lambda (x)
                           (cons (silver-brain--prop-id x) x))
                         items-seq)))

    ;; Insert outbound references.
    (dolist (reference (silver-brain--item-references-sort-by-target
                        (silver-brain-client-get-references (silver-brain--prop-references-out))))
      (silver-brain--item-insert-reference reference items))

    ;; Insert inbound references.
    (dolist (reference (silver-brain--item-references-sort-by-source
                        (silver-brain-client-get-references (silver-brain--prop-references-in))))
      (silver-brain--item-insert-reference reference items))))

(defun silver-brain--item-insert-reference (reference items)
  (widget-insert "  ")
  (silver-brain--widget-create-button
   "Edit" (lambda ()
            (let ((new-annotation (read-string "New annotation: ")))
              (silver-brain-client-update-reference (silver-brain--prop-id reference) new-annotation)
              (silver-brain-item-refresh-when-id-in (list (silver-brain--prop-source reference)
                                              (silver-brain--prop-target reference))))))
  (widget-insert " ")
  (silver-brain--widget-create-button
   "Del" (lambda ()
           (when (y-or-n-p "Delete this reference? ")
             (silver-brain-client-delete-reference (silver-brain--prop-id reference))
             (silver-brain-item-refresh-when-id-in (list (silver-brain--prop-source reference)
                                             (silver-brain--prop-target reference))))))
  (widget-insert " ")

  (if (string= (silver-brain--prop-source reference) (silver-brain--prop-id))
      (widget-insert (silver-brain--prop-name))
    (silver-brain--widget-create-item (silver-brain--prop (silver-brain--prop-target reference) items)))

  (widget-insert " -["
                 (silver-brain--prop-annotation reference)
                 "]-> ")

  (if (string= (silver-brain--prop-target reference) (silver-brain--prop-id))
      (widget-insert (silver-brain--prop-name))
    (silver-brain--widget-create-item (silver-brain--prop (silver-brain--prop-target reference) items)))

  (widget-insert "\n"))

(defun silver-brain--item-get-sorted-items (items)
  (seq-sort-by #'silver-brain--prop-id #'string< 
               (seq-sort-by #'silver-brain--prop-name #'string< items)))

(defun silver-brain--item-references-sort-by-source (references)
  (seq-sort-by (lambda (x) (silver-brain--prop-id (silver-brain--prop-source x)))
               #'string<
               (seq-sort-by (lambda (x) (silver-brain--prop-name (silver-brain--prop-source x)))
                            #'string<
                            references)))

(defun silver-brain--item-references-sort-by-target (references)
  (seq-sort-by (lambda (x) (silver-brain--prop-id (silver-brain--prop-target x)))
               #'string<
               (seq-sort-by (lambda (x) (silver-brain--prop-name (silver-brain--prop-target x)))
                            #'string<
                            references)))

;; ============================================================
;;  Refresh
;; ============================================================

(defun silver-brain-item-refresh-all ()
  (dolist (buffer (silver-brain--get-all-item-buffers))
    (with-current-buffer buffer
      (silver-brain-item-refresh))))

(defun silver-brain-item-refresh-when-id-in (ids)
  (dolist (buffer (silver-brain--get-all-item-buffers))
    (with-current-buffer buffer
      (when (member (silver-brain--prop-id) ids)
        (silver-brain-item-refresh)))))

;; ============================================================
;;  Commands
;; ============================================================

(cl-defun silver-brain-create-item (&optional name)
  "Create a new item. If NAME is given, it is used as the name
of new item. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Item name: ")))
         (item (silver-brain-client-create-item name silver-brain-default-content-type)))
    (silver-brain-hello-refresh)
    (silver-brain-item-show item)))

(cl-defun silver-brain-item-rename ()
  "Rename current item."
  (interactive)
  (silver-brain--verify-current-item)

  (let* ((new-name (read-string (format "Rename to: ")
                                (silver-brain--prop-name silver-brain-current-item)))
         (new-item (silver-brain--update-prop-name new-name)))
    (silver-brain-client-update-item (silver-brain--prop-id)
                         :name new-name)
    (silver-brain-item-refresh-all)
    (silver-brain-hello-refresh)))

(defun silver-brain-item-delete ()
  "Delete current item."
  (interactive)
  (silver-brain--verify-current-item)
  (silver-brain--delete-item silver-brain-current-item))

(defun silver-brain-item-update-content-type ()
  "Update content type of current item."
  (interactive)
  (silver-brain--verify-current-item)
  (let ((new-content-type (read-string "Content type: "
                                       (silver-brain--prop-content-type silver-brain-current-item))))
    (silver-brain-client-update-item (silver-brain--prop-id silver-brain-current-item)
                         :content-type new-content-type)
    (silver-brain-item-refresh)))

(defun silver-brain-item-refresh ()
  "Refresh current item."
  (interactive)
  (let* ((new-item (silver-brain-client-get-item (silver-brain--prop-id)))
         (new-buffer-name (silver-brain--item-get-buffer-name new-item)))
    (rename-buffer new-buffer-name)
    (silver-brain--item-prepare-buffer new-item)))

(provide 'silver-brain-item)
