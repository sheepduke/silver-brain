;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'iso8601)
(require 'dash)

(require 'silver-brain-vars)

(defun silver-brain-insert-h1 (&rest texts)
  "Insert TEXT and apply silver-brain-h1 face to it."
  (apply #'silver-brain-insert-with-face 'silver-brain-h1 texts))

(defun silver-brain-insert-h2 (&rest texts)
  "Insert TEXT and apply silver-brain-h1 face to it."
  (apply #'silver-brain-insert-with-face 'silver-brain-h2 texts))

(defun silver-brain-insert-with-face (face &rest texts)
  "Insert TEXT at point with FACE."
  (let ((start (point)))
    (apply #'insert texts)
    (put-text-property start (point) 'face face)))

(defun silver-brain-format-time (time-string)
  (format-time-string silver-brain-time-format (encode-time (iso8601-parse time-string))))

(defun silver-brain-completing-read (things key-fun value-fun)
  (let ((thing-map (--map (cons (funcall key-fun it)
                                (funcall value-fun it))
                          things)))
    (and things
         (cdr (assoc-string (completing-read "Choose: " thing-map)
                            thing-map)))))

(provide 'silver-brain-util)
