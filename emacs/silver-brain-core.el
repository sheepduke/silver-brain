;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defun silver-brain--assoc (alist keys)
  (cond
   ((atom keys) (alist-get keys alist nil nil #'string-equal))
   ((null (cdr keys)) (alist-get (car keys) alist nil nil #'string-equal))
   (t (silver-brain--assoc alist (cdr keys)))))

(defun silver-brain-alist-uuid (alist) (silver-brain--assoc alist "uuid"))

(defun silver-brain-alist-name (alist) (silver-brain--assoc alist "name"))

(defun silver-brain-alist-content-type (alist) (silver-brain--assoc alist "contentType"))

(defun silver-brain-alist-content (alist) (silver-brain--assoc alist "content"))

(defun silver-brain-alist-inbound-links (alist) (silver-brain--assoc alist "inboundLinks"))

(defun silver-brain-alist-outbound-links (alist) (silver-brain--assoc alist "outboundLinks"))

(defun silver-brain-alist-mutual-links (alist) (silver-brain--assoc alist "mutualLinks"))

(defun silver-brain-alist-create-time (alist) (silver-brain--assoc alist "createTime"))

(defun silver-brain-alist-update-time (alist) (silver-brain--assoc alist "updateTime"))

(defun silver-brain-alist-source (alist) (silver-brain--assoc alist "source"))

(defun silver-brain-alist-relation (alist) (silver-brain--assoc alist "relation"))

(defun silver-brain-alist-target (alist) (silver-brain--assoc alist "target"))

(defun silver-brain-alist-other (alist) (silver-brain--assoc alist "other"))

(provide 'silver-brain-core)
