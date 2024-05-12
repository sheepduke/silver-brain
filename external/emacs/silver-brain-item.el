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
    (define-key map (kbd "o") 'silver-brain-open)
    (define-key map (kbd "d") 'silver-brain-delete-item-at-point)
    (define-key map (kbd "D") 'silver-brain-item-delete)
    (define-key map (kbd "r") 'silver-brain-item-rename)
    (define-key map (kbd "u t") 'silver-brain-item-update-content-type)
    (define-key map (kbd "c i") 'silver-brain-create-item)
    (define-key map (kbd "c I") 'silver-brain-create-items)
    (define-key map (kbd "c p") 'silver-brain-item-add-parent)
    (define-key map (kbd "c c") 'silver-brain-item-add-child)
    (define-key map (kbd "c C") 'silver-brain-item-create-children)
    (define-key map (kbd "c p") 'silver-brain-item-add-parent)
    (define-key map (kbd "c P") 'silver-brain-item-create-parents)
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
  (silver-brain--item-insert-families t)
  
  ;; Insert children.
  (widget-insert "\n")
  (silver-brain--item-insert-families nil)

  ;; Insert siblings.
  (widget-insert "\n")
  (silver-brain--item-insert-siblings)

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

(defun silver-brain--item-insert-families (parentp)
  (silver-brain--widget-insert-with-face (if parentp "Parents " "Children ") 'silver-brain-h2)
  (silver-brain--widget-create-button
   "New" (lambda (&rest _)
           (if parentp
               (silver-brain-item-add-parent)
             (silver-brain-item-add-child))))

  (let ((others (silver-brain-client-get-items (if parentp
                                       (silver-brain--prop-parents)
                                     (silver-brain--prop-children)))))
    (unless (null others)
      (widget-insert "\n"))
    
    (widget-insert "\n  ")
    
    (dolist (other (silver-brain--item-get-sorted-items others))
      (silver-brain--widget-create-button
       "Del" (lambda (&rest _)
               (when (y-or-n-p "Confirm? ")
                 (let* ((this-id (silver-brain--prop-id))
                        (other-id (silver-brain--prop-id other)))
                   (if parentp
                       (silver-brain-client-delete-child other-id this-id)
                     (silver-brain-client-delete-child this-id other-id))
                   
                   (silver-brain-item-refresh-when-id-in (append (list this-id other-id)
                                                     (silver-brain--prop-siblings)))))))
      (widget-insert " ")
      (silver-brain--widget-create-item other)
      (widget-insert "\n  "))))

(defun silver-brain--item-insert-siblings ()
  (silver-brain--widget-insert-with-face "Siblings "
                             'silver-brain-h2)

  (let ((siblings (silver-brain-client-get-items (silver-brain--prop-siblings))))
    (unless (null siblings)
      (widget-insert "\n\n"))
    
    (widget-insert "  ")
    
    (dolist (sibling (silver-brain--item-get-sorted-items siblings))
      (when (>= (+ 2 (current-column) (length (silver-brain--prop-name sibling)))
               (window-width))
        (widget-insert "\n  "))
      (silver-brain--widget-create-item sibling)
      (widget-insert "  "))

    (widget-insert "\n")))

(defun silver-brain--item-insert-references ()
  (let* ((references-out (silver-brain-client-get-references (silver-brain--prop-references-out)))
         (references-in (silver-brain-client-get-references (silver-brain--prop-references-in)))
         (items-seq (silver-brain-client-get-items
                     (append (seq-map #'silver-brain--prop-target references-out)
                             (seq-map #'silver-brain--prop-source references-in))))
         (items (seq-map (lambda (x)
                           (cons (silver-brain--prop-id x) x))
                         items-seq)))

    ;; Insert outbound references.
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

    (dolist (reference (silver-brain--item-references-sort-by-target
                        (silver-brain-client-get-references (silver-brain--prop-references-out))))
      (silver-brain--item-insert-reference reference items))

    ;; Insert inbound references.
    (widget-insert "\n")
    (silver-brain--widget-insert-with-face "Referenced By" 'silver-brain-h2)
    (widget-insert "\n")
    (dolist (reference (silver-brain--item-references-sort-by-source
                        (silver-brain-client-get-references (silver-brain--prop-references-in))))
      (silver-brain--item-insert-reference reference items))))

(defun silver-brain--item-insert-reference (reference items)
  (widget-insert "\n  ")
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

  (widget-insert " -[" (silver-brain--prop-annotation reference) "]-> ")

  (if (string= (silver-brain--prop-target reference) (silver-brain--prop-id))
      (widget-insert (silver-brain--prop-name))
    (silver-brain--widget-create-item (silver-brain--prop (silver-brain--prop-target reference) items)))

  (widget-insert "\n"))

(defun silver-brain--item-get-sorted-items (items)
  (seq-sort-by #'silver-brain--prop-name #'string< 
               (seq-sort-by #'silver-brain--prop-id #'string< 
                            items)))

(defun silver-brain--item-references-sort-by-source (references)
  (seq-sort-by (lambda (x) (silver-brain--prop-name (silver-brain--prop-source x)))
               #'string<
               (seq-sort-by (lambda (x) (silver-brain--prop-id (silver-brain--prop-source x)))
                            #'string<
                            references)))

(defun silver-brain--item-references-sort-by-target (references)
  (seq-sort-by (lambda (x) (silver-brain--prop-name (silver-brain--prop-target x)))
               #'string<
               (seq-sort-by (lambda (x) (silver-brain--prop-id (silver-brain--prop-target x)))
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

(cl-defun silver-brain-create-item ()
  "Create a new item. It prompts the user to input name."
  (interactive)
  (let* ((name (read-string "Item name: "))
         (item (silver-brain-client-get-item
                (silver-brain-client-create-item name silver-brain-default-content-type))))
    (silver-brain-hello-refresh)
    (silver-brain-item-show item)))

(cl-defun silver-brain-create-items ()
  "Create new items in batch. It prompts the user to input name.
Input an empty string to finish."
  (interactive)
  (let* (name (count 0))
    (while (not (seq-empty-p (setq name (read-string "Item name: "))))
      (silver-brain-client-create-item name silver-brain-default-content-type)
      (cl-incf count))
    (message "Created %d items." count))
    (silver-brain-hello-refresh))

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

(cl-defun silver-brain-item-add-parent (&optional (create-if-not-exists-p t))
  "Search for an item and set it as a parent.

If nothing is found and CREATE-IF-NOT-EXISTS-P is set to T (default),
create a new item."
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((this-id (silver-brain--prop-id))
         (parent-id (silver-brain--search-items-and-select
                     (read-string (format "Search for parent: "))
                     create-if-not-exists-p)))
    (when parent-id
      (silver-brain-client-add-child parent-id this-id)
      (silver-brain-item-refresh-when-id-in (list this-id parent-id)))))

(defun silver-brain-item-create-parents ()
  "Create items and set them as the parent of current item.
This function will continuously prompt for new items. Input empty string to stop."
  (interactive)
  (silver-brain--verify-current-item)
  (let (name)
    (while (not (seq-empty-p (setq name (read-string "Create parent: "))))
      (silver-brain-client-add-child (silver-brain-client-create-item name silver-brain-default-content-type)
                         (silver-brain--prop-id))))
  (silver-brain-item-refresh))

(defun silver-brain-item-create-children ()
  "Create items and set them as the parent of current item.
This function will continuously prompt for new items. Input empty string to stop."
  (interactive)
  (let (name)
    (while (not (seq-empty-p (setq name (read-string "Create child: "))))
      (silver-brain-client-add-child (silver-brain--prop-id)
                         (silver-brain-client-create-item name silver-brain-default-content-type))))
  (silver-brain-item-refresh))

(defun silver-brain-item-add-child ()
  (interactive)
  (let* ((this-id (silver-brain--prop-id))
         (child-id (silver-brain--search-items-and-select
                    (read-string (format "Search for child: ")))))
    (silver-brain-client-add-child this-id child-id)
    (silver-brain-item-refresh-when-id-in (list this-id child-id))))

(defun silver-brain-item-refresh ()
  "Refresh current item."
  (interactive)
  (let* ((new-item (silver-brain-client-get-item (silver-brain--prop-id)))
         (new-buffer-name (silver-brain--item-get-buffer-name new-item)))
    (rename-buffer new-buffer-name)
    (silver-brain--item-prepare-buffer new-item)))

(provide 'silver-brain-item)
