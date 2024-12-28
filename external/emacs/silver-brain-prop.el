;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defun silver-brain-prop-id (&optional obj)
  (silver-brain-prop "id" obj))

(defun silver-brain-prop-name (&optional obj)
  (silver-brain-prop "name" obj))

(defun silver-brain-prop-update-name (value &optional obj)
  (silver-brain-prop-update "name" value obj))

(defun silver-brain-prop-content-type (&optional obj)
  (silver-brain-prop "contentType" obj))

(defun silver-brain-prop-update-content-type (value &optional obj)
  (silver-brain-prop-update "contentType" value obj))

(defun silver-brain-prop-content (&optional obj)
  (silver-brain-prop "content" obj))

(defun silver-brain-prop-parents (&optional obj)
  (silver-brain-prop "parents" obj))

(defun silver-brain-prop-children (&optional obj)
  (silver-brain-prop "children" obj))

(defun silver-brain-prop-siblings (&optional obj)
  (silver-brain-prop "siblings" obj))

(defun silver-brain-prop-references-out (&optional obj)
  (silver-brain-prop "referencesFromThis" obj))

(defun silver-brain-prop-references-in (&optional obj)
  (silver-brain-prop "referencesToThis" obj))

(defun silver-brain-prop-source (&optional obj)
  (silver-brain-prop "source" obj))

(defun silver-brain-prop-target (&optional obj)
  (silver-brain-prop "target" obj))

(defun silver-brain-prop-annotation (&optional obj)
  (silver-brain-prop "annotation" obj))

(defun silver-brain-prop-update-content (value &optional obj)
  (silver-brain-prop-update "content" value obj))

(defun silver-brain-prop-create-time (&optional obj)
  (silver-brain-prop "createTime" obj))

(defun silver-brain-prop-update-time (&optional obj)
  (silver-brain-prop "updateTime" obj))

(defun silver-brain-prop (key &optional obj)
  (let ((obj (or obj silver-brain-current-item)))
    (cdr (assoc-string key obj))))

(defun silver-brain-prop-update (key value &optional obj)
  (let ((obj (or obj silver-brain-current-item)))
    (cons (cons key value)
          (assoc-delete-all key obj #'string=))))

(provide 'silver-brain-prop)
