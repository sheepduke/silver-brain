;;; Package --- silver-brain-search

;;; Commentary:
;;;
;;; Search concepts by keywords.

;;; Code:

(require 'silver-brain-util)

(defvar silver-brain-search-buffer-name "*Silver Brain - Search*"
  "Buffer name used for searching concepts.")

(defvar silver-brain-search-function 'silver-brain-search
  "The function used to search concepts.")

(defun silver-brain-search (query)
  "Search concept."
  (let* ((concepts (silver-brain-api--search-concept query))
         (candidates (mapcar 'silver-brain-search--concept-to-candidate concepts)))
    (cond
     ((null candidates) (message "No concept found."))
     (t (let* ((candidate (completing-read "Select: " candidates)))
          (silver-brain-search--candidate-to-uuid candidate))))))

(defun silver-brain-search--concept-to-candidate (concept)
  "Convert CONCEPT to candidate string."
  (concat (silver-brain-concept-name concept)
          " | "
          (silver-brain-concept-uuid concept)))

(defun silver-brain-search--candidate-to-uuid (candidate)
  "Extract UUID from CANDIDATE.
Returns a string if the UUID is found, NIL otherwise."
  (let* ((match (string-match "| [A-Za-z0-9-]+$" candidate)))
    (if match
        (substring candidate (+ 2 match))
      nil)))

(provide 'silver-brain-search)

;;; silver-brain-search.el ends here
