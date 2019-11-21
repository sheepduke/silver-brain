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
        ((concept-childp target source)
         (become-friend source target))
        ;; Otherwise, make child.
        (t
         (become-child source target))))))

(defun purge ()
  "Remove everything."
  (setf *concept-map* (make-hash-table :test #'equal))
  (db::purge))

(defun get-all-concepts ()
  "Return all concepts as a list."
  (hash-table-values *concept-map*))

(defun get-concept-by-uuid (uuid)
  "Return corresponding UUID from the cache."
  (gethash uuid *concept-map*))

(defun find-concept-by-name (name)
  (remove-if-not (lambda (concept)
                   (str:containsp name (core:concept-name concept) :ignore-case t))
                 (hash-table-values *concept-map*)))

(defun create-concept (name content content-format)
  "Create a new concept."
  (let ((concept (make-instance 'concept
                                :name name
                                :content content
                                :content-format content-format)))
    (setf (gethash (concept-uuid concept) *concept-map*) concept)
    (db:save-concept (concept-uuid concept)
                     :name name
                     :content content
                     :content-format content-format)
    concept))

(defun update-concept (concept &key name content content-format)
  "Update corresponding concept in the map with CONCEPT, by UUID.
The UUID must be valid."
    (setf (concept-name concept) name)
    (setf (concept-content concept) content)
    (setf (concept-content-format concept) content-format)
    (setf (gethash (concept-uuid concept) *concept-map*) concept)
    (db:save-concept (concept-uuid concept)
                     :name name
                     :content content
                     :content-format content-format))

(defun delete-concept (uuid)
  "Delete given CONCEPT from the map."
  (remhash uuid *concept-map*)
  (db:delete-concept uuid))
