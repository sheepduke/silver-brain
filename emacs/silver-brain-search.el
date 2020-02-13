;;; Package --- silver-brain-search

;;; Commentary:
;;;
;;; Search concepts by keywords.

;;; Code:

(require 'silver-brain-util)

(defvar silver-brain-search-function 'silver-brain-search-default
  "The function used to search concepts.")

(cl-defun silver-brain-search (&key prompt)
  (funcall silver-brain-search-function :prompt prompt))

;;;###autoload
(cl-defun silver-brain-search-default (&key (prompt "Search: ")
                                            (create t))
  "Search concept from QUERY and return UUID.
PROMPT is the prompt when reading user input.
When CREATE is T, create the "
  (let* ((concepts (silver-brain-api--get-all-concepts))
         (candidates (mapcar 'silver-brain-search--concept-to-candidate concepts)))
    (cond
     ((null candidates) (message "No concept found.") nil)
     (t (let* ((candidate (completing-read "Select: " candidates)))
          (silver-brain-search--select-or-create candidate :create create))))))

(cl-defun silver-brain-search--select-or-create (candidate &key (create t))
  "Given a search CANDIDATE, return the corresponding UUID.
If the candidate concept does not exist, ask for creating."
  (let ((selection (silver-brain-search--candidate-to-uuid candidate)))
    (cond
     (selection selection)
     ((and create (y-or-n-p "Concept does not exist, create? "))
      (silver-brain-concept-uuid
       (silver-brain-api--create-concept
        candidate
        silver-brain-default-content-format)))
     (t nil))))

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
