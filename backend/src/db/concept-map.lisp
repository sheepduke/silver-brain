(in-package silver-brain.service)

(defvar *concept-map* (make-hash-table :test #'equal)
  "The map that caches all the concepts and their relationship.")

(defun setup ()
  "Setup the concept map."
  (let* ((db-concepts (db:read-all-concepts))
         (relations (db:read-all-concept-relations)))
    (iter (for concept in db-concepts)
      (setf (gethash (db:concept-uuid concept) *concept-map*)
            (db:db-concept-to-core-concept concept)))
    (iter (for relation in relations)
      (for source = (gethash (db:concept-relation-source relation) *concept-map*))
      (for target = (gethash (db:concept-relation-target relation) *concept-map*))
      (cond
        ;; If any concept does not exist, ignore it.
        ((or (null source) (null target))
         (log:warn "Invalid record: ~a" relation))
        ;; If the target is the parent of source, make them friends.
        ((concept-childp source target)
         (become-friend source target))
        ;; Otherwise, make child.
        (t
         (become-child target source))))))

(defun get-all-concepts ()
  "Return all concepts as a list."
  (hash-table-values *concept-map*))

(defun get-concept-by-uuid (uuid)
  "Return corresponding UUID from the cache."
  (gethash uuid *concept-map*))

(defun find-concept-by-name (name)
  (remove-if-not (lambda (concept)
                   (str:containsp name (concept-name concept)))
                 (hash-table-values *concept-map*)))

(defun create-concept (name content content-format)
  "Add given CONCEPT to the map."
  (let* ((db-concept (make-instance 'concept
                                    :name name
                                    :content content
                                    :content-format content-format))
         (concept (db:db-concept-to-core-concept db-concept)))
    (db:save-concept db-concept)
    (setf (gethash (concept-uuid concept) *concept-map*) concept)))

(defun update-concept (uuid &key name content content-format)
  "Update corresponding concept in the map with CONCEPT, by UUID.
The UUID must be valid."
  (let ((concept (get-concept-by-uuid uuid)))
    (setf (concept-name concept) name)
    (setf (concept-content concept) content)
    (setf (concept-content-format concept) content-format)
    (db:save-concept uuid
                     :name name
                     :content content
                     :content-format content-format)))

(defun delete-concept (concept)
  "Delete given CONCEPT from the map."
  (let ((uuid (concept-uuid concept)))
    (remhash uuid *concept-map*)
    (db:delete-concept uuid)))

(defun make-child (concept child)
  "Make CHILD a child of CONCEPT."
  (core:become-child concept child)
  (db:add-relation (concept-uuid concept) (concept-uuid child)))

(defun make-friend (concept friend)
  "Make them friends."
  (core:become-friend concept friend)
  (db:add-relation (concept-uuid concept) (concept-uuid friend))
  (db:add-relation (concept-uuid friend) (concept-uuid concept)))

(defun remove-relation (c1 c2)
  "Remove any relation between c1 and c2."
  (remove-relations-between c1 c2))
