;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'widget)
(require 'wid-edit)
(require 'seq)

(require 'silver-brain-vars)
(require 'silver-brain-util)
(require 'silver-brain-client)
(require 'silver-brain-item-content)

(defvar silver-brain-item-buffer-name-format "*SB/Item - %s*")

(defvar-local silver-brain-current-item nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-parents nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-children nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

(defvar-local silver-brain-item-references nil)
(put 'silver-brain-current-item 'permanently-enabled-local-variables t)

;; ============================================================
;;  Mode
;; ============================================================

(define-derived-mode silver-brain-item-mode fundamental-mode "SB/Item"
  "Major mode for Silver Brain single item."

  (let ((keymap (make-composed-keymap (list (make-sparse-keymap)
                                            widget-keymap))))
    (define-key keymap (kbd "g") #'silver-brain-item-refresh)
    (define-key keymap (kbd "q") #'silver-brain-item-quit)
    (define-key keymap (kbd "j") #'silver-brain-widget-jump)

    (define-key keymap (kbd "s o") #'silver-brain-search-and-open-item)
    (define-key keymap (kbd "s l") #'silver-brain-search)

    (define-key keymap (kbd "e") #'silver-brain-item-open-content)
    (define-key keymap (kbd "i c") #'silver-brain-item-create-item)
    (define-key keymap (kbd "i r") #'silver-brain-item-rename)
    (define-key keymap (kbd "i t") #'silver-brain-item-update-content-type)
    (define-key keymap (kbd "i d") #'silver-brain-item-delete)

    (define-key keymap (kbd "p a") #'silver-brain-item-add-parent)
    (define-key keymap (kbd "p c") #'silver-brain-item-create-parent)
    (define-key keymap (kbd "p d") #'silver-brain-item-delete-parent)

    (define-key keymap (kbd "c a") #'silver-brain-item-add-child)
    (define-key keymap (kbd "c c") #'silver-brain-item-create-child)
    (define-key keymap (kbd "c d") #'silver-brain-item-delete-child)

    (define-key keymap (kbd "r a") #'silver-brain-item-add-reference)
    (define-key keymap (kbd "r d") #'silver-brain-item-delete-reference)

    (define-key keymap (kbd "SPC") 'silver-brain-item-hydra/body)

    (setq silver-brain-item-mode-map keymap)))

(pretty-hydra-define silver-brain-item-hydra (:color blue)
  ("Buffer"
   (("o" silver-brain-search-and-open-item "open")
    ("j" silver-brain-widget-jump "jump")
    ("g" silver-brain-item-refresh "refresh")
    ("q" silver-brain-item-quit "close"))

   "Groups"
   (("i" silver-brain-item-basic-hydra/body "item")
    ("s" silver-brain-item-search-hydra/body "search")
    ("p" silver-brain-item-parent-hydra/body "parent")
    ("c" silver-brain-item-child-hydra/body "child")
    ("r" silver-brain-item-reference-hydra/body "reference"))))

(pretty-hydra-define silver-brain-item-basic-hydra ()
  ("Item"
   (("e" silver-brain-item-open-content "edit content")
    ("c" silver-brain-item-create-item "create")
    ("r" silver-brain-item-rename "rename")
    ("t" silver-brain-item-update-content-type "update content type")
    ("d" silver-brain-item-delete "delete this"))))

(pretty-hydra-define silver-brain-item-search-hydra ()
  ("Search"
   (("o" silver-brain-search-and-open-item "open")
    ("l" silver-brain-list-items "search"))))

(pretty-hydra-define silver-brain-item-parent-hydra ()
  ("Parent"
   (("a" silver-brain-item-add-parent "add")
    ("c" silver-brain-item-create-parent "create")
    ("d" silver-brain-item-delete-parent "delete"))))

(pretty-hydra-define silver-brain-item-child-hydra ()
  ("Child"
   (("a" silver-brain-item-add-child "add")
    ("c" silver-brain-item-create-child "create")
    ("d" silver-brain-item-delete-child "delete"))))

(pretty-hydra-define silver-brain-item-reference-hydra ()
  ("Reference"
   (("a" silver-brain-item-add-reference "add")
    ("c" silver-brain-item-create-reference "create")
    ("d" silver-brain-item-delete-reference "delete"))))

;; ============================================================
;;  Buffer
;; ============================================================

(defun silver-brain-search-and-open-item (&optional search-string)
  "Search and open concept."
  (interactive "sSearch item: ")
  (silver-brain-item-open (silver-brain--search-items-and-select search-string)))

(defun silver-brain-item-open (id)
  (pop-to-buffer-same-window (silver-brain-item-setup id)))

(defun silver-brain-item-setup (id)
  (let* ((item (silver-brain-client-get-item id))
         (buffer (get-buffer-create (format silver-brain-item-buffer-name-format (silver-brain--prop-name item)))))
    (silver-brain--with-widget-buffer buffer 
      (silver-brain-item-mode)
      (setq silver-brain-current-item item)

      ;; Fetch parents.
      (setq silver-brain-item-parents
            (silver-brain--item-get-sorted-items (silver-brain-client-get-items (silver-brain--prop-parents item))))

      ;; Fetch children.
      (setq silver-brain-item-children
            (silver-brain--item-get-sorted-items (silver-brain-client-get-items (silver-brain--prop-children item))))

      ;; Fetch references.
      (setq silver-brain-item-references (silver-brain-client-get-references (silver-brain--prop-references-out item)))

      (silver-brain--item-insert-widgets item))

    buffer))

(defun silver-brain--item-insert-widgets (item)
  (silver-brain--widget-insert-with-face (silver-brain--prop-name silver-brain-current-item) 'silver-brain-h1)

  (widget-insert "\n\n"
                 "  Content Type: "
                 (silver-brain--prop-content-type))

  (widget-insert "\n  Create Time: "
                 (silver-brain--format-time (silver-brain--prop-create-time item))
                 "\n  Update Time: "
                 (silver-brain--format-time (silver-brain--prop-update-time item))
                 "\n")
  
  ;; Insert parents.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Parents" 'silver-brain-h2)
  (widget-insert (if silver-brain-item-parents "\n\n" "\n"))

  (dolist (parent silver-brain-item-parents)
    (widget-insert "  ")
    (silver-brain--widget-create-item parent)
    (widget-insert "\n"))

  ;; Insert children.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Children" 'silver-brain-h2)
  (widget-insert (if silver-brain-item-children "\n\n" "\n"))

  (dolist (child silver-brain-item-children)
    (widget-insert "  ")
    (silver-brain--widget-create-item child)
    (widget-insert "\n"))

  ;; Insert siblings.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Siblings" 'silver-brain-h2)

  (let ((siblings (silver-brain--item-get-sorted-items
                   (silver-brain-client-get-items (silver-brain--prop-siblings)))))
    (unless (null siblings)
      (widget-insert "\n\n")
      (widget-insert "  "))
    
    (dolist (sibling siblings)
      (when (>= (+ 2 (current-column) (length (silver-brain--prop-name sibling)))
               (window-width))
        (widget-insert "\n  "))
      (silver-brain--widget-create-item sibling)
      (widget-insert "  "))

    (widget-insert "\n"))

  ;; Insert references.
  (widget-insert "\n")
  (silver-brain--item-insert-references)

  ;; Insert content.
  (widget-insert "\n")
  (silver-brain--widget-insert-with-face "Content\n" 'silver-brain-h2)
  (widget-insert (make-horizontal-bar)
                 "\n"
                 (or (silver-brain--prop-content item) "")))

(cl-defun make-horizontal-bar (&optional length)
  (let ((bar-length (or length
                        (1- (window-width)))))
    (string-join (cl-loop for i from 1 to bar-length collect "⎯"))))

(defun silver-brain--verify-current-item ()
  (unless silver-brain-current-item
    (error "This command must be invoked within a item buffer")))

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
    (silver-brain--widget-insert-with-face "References" 'silver-brain-h2)
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
;;  Commands
;; ============================================================

(cl-defun silver-brain-item-quit ()
  (interactive)
  (silver-brain--verify-current-item)
  (condition-case nil
      (kill-buffer (silver-brain-item-get-content-buffer-name))
    (error nil))
  (kill-this-buffer))

(cl-defun silver-brain-create-and-open-item ()
  "Create a new item and open it. It prompts the user to input name."
  (interactive)
  (silver-brain-item-open (silver-brain--create-item)))

(cl-defun silver-brain-item-rename ()
  "Rename current item."
  (interactive)
  (silver-brain--verify-current-item)

  (let* ((item-id (silver-brain--prop-id))
         (new-name (read-string (format "Rename to: ")
                                (silver-brain--prop-name silver-brain-current-item))))
    (silver-brain-client-update-item item-id :name new-name)
    (rename-buffer (format silver-brain-item-buffer-name-format new-name))
    (silver-brain-item-refresh-all)))

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

(defun silver-brain-item-add-parent ()
  "Search for an item and set it as a parent."
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((this-id (silver-brain--prop-id))
         (parent-id (silver-brain--search-items-and-select
                     (read-string (format "Search for parent: ")))))
    (when parent-id
      (silver-brain-client-add-child parent-id this-id)
      (silver-brain-item-refresh-when-id-in (list this-id parent-id)))))

(defun silver-brain-item-create-parent ()
  "Create an item and set it as a parent."
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((this-id (silver-brain--prop-id))
         (parent-id (silver-brain--create-item)))
    (when parent-id
      (silver-brain-client-add-child parent-id this-id)
      (silver-brain-item-refresh-when-id-in (list this-id parent-id)))))

(defun silver-brain-item-delete-parent ()
  "Select a parent and delete the relationship."
  (interactive)
  (let ((this-id (silver-brain--prop-id))
        (parent-id (silver-brain--select-item silver-brain-item-parents)))
    (silver-brain-client-delete-child parent-id this-id)
    (silver-brain-item-refresh-when-id-in (list this-id parent-id))))

(defun silver-brain-item-add-child ()
  "Search for an item and set it as a parent."
  (interactive)
  (let* ((this-id (silver-brain--prop-id))
         (child-id (silver-brain--search-items-and-select
                    (read-string (format "Search for child: ")))))
    (when child-id
      (silver-brain-client-add-child this-id child-id)
      (silver-brain-item-refresh-when-id-in (list this-id child-id)))))

(defun silver-brain-item-create-child ()
  "Create an item and set it as a parent."
  (interactive)
  (silver-brain--verify-current-item)
  (let* ((this-id (silver-brain--prop-id))
         (child-id (silver-brain--create-item)))
    (when child-id
      (silver-brain-client-add-child this-id child-id)
      (silver-brain-item-refresh-when-id-in (list this-id child-id)))))

(defun silver-brain-item-delete-child ()
  "Select a child and delete the relationship."
  (interactive)
  (let ((this-id (silver-brain--prop-id))
        (child-id (silver-brain--select-item silver-brain-item-children)))
    (silver-brain-client-delete-child this-id child-id)
    (silver-brain-item-refresh-when-id-in (list this-id child-id))))

(defun silver-brain-item-add-reference ()
  (interactive)
  (let* ((source (silver-brain--prop-id))
         (target (silver-brain--search-items-and-select
                  (read-string "Search for target item: "))))
    (when target
      (let ((annotation (read-string "Annotation: ")))
        (silver-brain-client-create-reference source target annotation)
        (silver-brain-item-refresh-when-id-in (list source target))))))

(defun silver-brain-item-delete-reference ()
  (interactive)
  (let ((reference (silver-brain--item-select-reference)))
    (silver-brain-client-delete-reference (silver-brain--prop-id reference))
    (silver-brain-item-refresh-when-id-in (list (silver-brain--prop-source reference)
                                    (silver-brain--prop-target reference)))))

(defun silver-brain-item-refresh (&optional id)
  "Refresh current item."
  (interactive)
  (silver-brain-item-setup (or id (silver-brain--prop-id))))

;; ============================================================
;;  Internal Functions
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

(defun silver-brain--item-select-reference ()
  (let* ((reference-map (seq-map (lambda (reference)
                                   (cons (format "This -[%s]-> %s"
                                                 (silver-brain--prop-annotation reference)
                                                 (silver-brain--prop-target reference))
                                         reference))
                                 silver-brain-item-references))
         (selection (completing-read "Choose reference: " reference-map))
         (selected-reference (cdr (assoc-string selection reference-map))))
    selected-reference))

(provide 'silver-brain-item)
