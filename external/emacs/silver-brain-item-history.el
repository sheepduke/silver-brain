;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defvar silver-brain-item-history '()
  "The history of opened item ids.")

(defvar silver-brain-item-history-cursor -1
  "The current index of the history.")

(defun silver-brain-item-history-initialize ()
  "Initialize the item history."
  (interactive)
  (setq silver-brain-item-history '())
  (setq silver-brain-item-history-cursor -1))

(defun silver-brain-item-history-add (item-id)
  ;; If the cursor is not pointing to the last one, cut the history.
  (when (< silver-brain-item-history-cursor (1- (length silver-brain-item-history)))
    (setq silver-brain-item-history (cl-subseq silver-brain-item-history 0 (1+ silver-brain-item-history-cursor))))

  ;; If the history is full, remove the oldest items.
  (while (>= (length silver-brain-item-history) silver-brain-item-history-size)
    (pop silver-brain-item-history)
    (when (> silver-brain-item-history-cursor 0)
      (cl-decf silver-brain-item-history-cursor)))

  ;; Add the new item id to the history and increase the cursor.
  (setq silver-brain-item-history
        (append silver-brain-item-history (list item-id)))
  (cl-incf silver-brain-item-history-cursor))

(defun silver-brain-item-history-move-backward ()
  "Move to the previous item id."
  (interactive)
  (when (<= silver-brain-item-history-cursor 0)
    (error "No previous item in history"))
  (cl-decf silver-brain-item-history-cursor)
  (elt silver-brain-item-history silver-brain-item-history-cursor))

(defun silver-brain-item-history-move-forward ()
  "Move to the next item id."
  (interactive)
  (when (>= silver-brain-item-history-cursor (1- (length silver-brain-item-history)))
    (error "No next item in history"))
  (cl-incf silver-brain-item-history-cursor)
  (elt silver-brain-item-history silver-brain-item-history-cursor))

(provide 'silver-brain-item-history)
