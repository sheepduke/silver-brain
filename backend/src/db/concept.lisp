(uiop:define-package silver-brain/db/concept
  (:nicknames db/concept)
  (:use #:cl
        #:silver-brain/db/concept-dao
        #:silver-brain/db/concept-relation-dao)
  (:import-from #:sxql
                #:where)
  (:reexport #:silver-brain/db/concept-dao)
  (:export #:insert #:save #:get-by-uuid #:get-all
           #:find-by-name #:erase))

(in-package silver-brain/db/concept)

(defun insert (name content content-format)
  "Add given `concept` to database."
  (let ((concept (make-instance 'concept
                                :name name
                                :content content
                                :content-format content-format)))
    (mito:save-dao concept)
    concept))

(defun save (concept)
  "Save `concept` to database."
  (mito:save-dao concept))

(defun get-by-uuid (uuid)
  "Get concept instance by its UUID."
  (mito:find-dao 'concept :uuid uuid))

(defun get-all ()
  "Return a list UUID and name of all concepts in a list of assoc list.
The keys of each alist is `(:id :name)`."
  (mito:select-dao 'concept))

(defun find-by-name (search)
  "Search the concept by its name."
  (mito:select-dao 'concept
    (where (:like :name (format nil "%~a%" search)))))

(defun erase (concept)
  "Delete given CONCEPT from database."
  (mito:delete-dao concept)
  (mito:delete-by-values ' concept-relation :source (uuid concept))
  (mito:delete-by-values 'concept-relation :target (uuid concept)))
