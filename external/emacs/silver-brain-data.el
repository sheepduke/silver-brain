;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(defun silver-brain--get-id (obj)
  (silver-brain--get "id" obj))

(defun silver-brain--get-name (obj)
  (silver-brain--get "name" obj))

(defun silver-brain--get-content-type (obj)
  (silver-brain--get "contentType" obj))

(defun silver-brain--get-content (obj)
  (silver-brain--get "content" obj))

(defun silver-brain--get-create-time (obj)
  (silver-brain--get "createTime" obj))

(defun silver-brain--get-update-time (obj)
  (silver-brain--get "updateTime" obj))

(defun silver-brain--get (key obj)
  (cdr (assoc-string key obj)))

(provide 'silver-brain-data)
