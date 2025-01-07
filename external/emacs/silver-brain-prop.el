;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defun silver-brain-prop-id (obj)
  (silver-brain-prop "id" obj))

(defun silver-brain-prop-name (obj)
  (silver-brain-prop "name" obj))

(defun silver-brain-prop-update-name (value obj)
  (silver-brain-prop-update "name" value obj))

(defun silver-brain-prop-content-type (obj)
  (silver-brain-prop "contentType" obj))

(defun silver-brain-prop-update-content-type (value obj)
  (silver-brain-prop-update "contentType" value obj))

(defun silver-brain-prop-content (obj)
  (silver-brain-prop "content" obj))

(defun silver-brain-prop-properties (obj)
  (silver-brain-prop "properties" obj))

(defun silver-brain-prop-key (obj)
  (silver-brain-prop "key" obj))

(defun silver-brain-prop-value (obj)
  (silver-brain-prop "value" obj))

(defun silver-brain-prop-parents (obj)
  (silver-brain-prop "parents" obj))

(defun silver-brain-prop-children (obj)
  (silver-brain-prop "children" obj))

(defun silver-brain-prop-siblings (obj)
  (silver-brain-prop "siblings" obj))

(defun silver-brain-prop-references-out (obj)
  (silver-brain-prop "referencesFromThis" obj))

(defun silver-brain-prop-references-in (obj)
  (silver-brain-prop "referencesToThis" obj))

(defun silver-brain-prop-source (obj)
  (silver-brain-prop "source" obj))

(defun silver-brain-prop-target (obj)
  (silver-brain-prop "target" obj))

(defun silver-brain-prop-annotation (obj)
  (silver-brain-prop "annotation" obj))

(defun silver-brain-prop-update-content (value obj)
  (silver-brain-prop-update "content" value obj))

(defun silver-brain-prop-create-time (obj)
  (silver-brain-prop "createTime" obj))

(defun silver-brain-prop-update-time (obj)
  (silver-brain-prop "updateTime" obj))

(defun silver-brain-prop (key obj)
  (let ((obj (or obj silver-brain-current-item)))
    (cdr (assoc-string key obj))))

(defun silver-brain-prop-update (key value obj)
  (cons (cons key value)
        (assoc-delete-all key obj #'string=)))

(provide 'silver-brain-prop)
