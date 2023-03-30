(in-package #:silver-brain.concept-map)

(defun search-concepts (search)
  (pipe (string:split search #\space :remove-empty? t)
        (search-name-alias)
        (list:map (op (mito:find-dao 'store:concept :uuid _)))))

(defun search-name-alias (tokens)
  (let* ((conditions (make-search-name-alias-conditions tokens)))
    (pipe (sxql:select (:distinct :uuid)
            (sxql:from :concept)
            (sxql:left-join :concept_alias
                            :on (:= :concept.uuid
                                 :concept_alias.concept_uuid))
            (sxql:where conditions))
          (mito:retrieve-by-sql)
          (list:map (op (plist:elt _ :uuid))))))

(defun make-search-name-alias-conditions (tokens &optional searched-uuids)
  (let ((conditions (pipe tokens
                          (list:map (op (format nil "%~A%" _)))
                          (list:map (fun (token)
                                      (list :or
                                            (list :like :name token)
                                            (list :like :alias token))))
                          (list:concat (list :and) _))))
    (if searched-uuids
        (list:push-back conditions
                        (list :in :uuid searched-uuids))
        conditions)))
