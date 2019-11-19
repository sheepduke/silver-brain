(in-package silver-brain.db)

(defun read-all-concept-relations ()
  "Return all relation objects."
  (mito:select-dao 'concept-relation))

;; (defun linkedp (source target)
;;   "Return T if there is a link from `source` to `target`."
;;   (uuid-linkedp (db/concept-dao:uuid source) (db/concept-dao:uuid target)))

;; (defun uuid-linkedp (source-uuid target-uuid)
;;   "Return T if concepts denoted by given UUID are linked."
;;   (if (mito:select-dao 'concept-relation
;;         (where (:and (:= :source source-uuid)
;;                      (:= :target target-uuid))))
;;       t
;;       nil))

;; (defun childp (child concept)
;;   "Return T if CHILD is a child of CONCEPT."
;;   (and (linkedp concept child)
;;        (not (linkedp child concept))))

;; (defun friendp (concept1 concept2)
;;   "Return T if CONCEPT1 and CONCEPT2 are friends."
;;   (and (linkedp concept1 concept2)
;;        (linkedp concept2 concept1)))

;; (defun get-parents-uuid (concept)
;;   "Return a list of CONCEPT's parents' UUID."
;;   (-<>> (mito:select-dao 'concept-relation
;;           (where (:= :target (db/concept-dao:uuid concept))))
;;     (mapcar #'source)
;;     (remove-if (lambda (uuid) (uuid-linkedp (db/concept-dao:uuid concept) uuid)))
;;     (mapcar #'db/concept:get-by-uuid)))

;; (defun get-children-uuid (concept)
;;   "Return a list of CONCEPT's children' UUID."
;;   (-<>> (mito:select-dao 'concept-relation
;;           (where (:= :source (db/concept-dao:uuid concept))))
;;     (mapcar #'target)
;;     (remove-if (lambda (uuid) (uuid-linkedp uuid (db/concept-dao:uuid concept))))
;;     (mapcar #'db/concept:get-by-uuid)))

;; (defun get-friends-uuid (concept)
;;   "Return a list of CONCEPT's friends' UUID."
;;   (-<>> (mito:select-dao 'concept-relation
;;           (where (:= :source (db/concept-dao:uuid concept))))
;;     (mapcar #'target)
;;     (remove-if-not (lambda (uuid) (uuid-linkedp uuid (db/concept-dao:uuid concept))))
;;     (mapcar #'db/concept:get-by-uuid)))

;; (defun unlink (concept1 concept2)
;;   "Remove any relationships between CONCEPT1 and CONCEPT2."
;;   (mito:delete-by-values 'concept-relation
;;                          :source (db/concept-dao:uuid concept1)
;;                          :target (db/concept-dao:uuid concept2))
;;   (mito:delete-by-values 'concept-relation
;;                          :source (db/concept-dao:uuid concept2)
;;                          :target (db/concept-dao:uuid concept1)))

(defun delete-relations-between (uuid1 uuid2)
  "Delete all relations between CONCEPT1 and CONCEPT2."
  (mito:delete-by-values 'concept-relation
                         :source uuid1
                         :target uuid2)
  (mito:delete-by-values 'concept-relation
                         :source uuid2
                         :target uuid1))

(defun delete-relations-of (uuid)
  "Delete all relations related to UUID."
  (mito:delete-by-values 'concept-relation :source uuid)
  (mito:delete-by-values 'concept-relation :target uuid))

(defun add-relation (source-uuid target-uuid)
  "Add a relation from SOURCE-UUID to TARGET-UUID."
  (mito:insert-dao (make-instance 'concept-relation
                                  :source source-uuid
                                  :target target-uuid)))

;; (defun become-child (child concept)
;;   "Make CHILD a child of CONCEPT."
;;   (delete-relations-between child concept)
;;   (mito:insert-dao
;;    (make-instance 'concept-relation
;;                   :source (concept-uuid concept)
;;                   :target (concept-uuid child))))

;; (defun become-friend (concept1 concept2)
;;   "Make CONCEPT1 and CONCEPT2 friends of each other."
;;   (delete-relations-between concept1 concept2)
;;   (mito:insert-dao
;;    (make-instance 'concept-relation
;;                   :source (concept-uuid concept1)
;;                   :target (concept-uuid concept2)))
;;   (mito:insert-dao
;;    (make-instance 'concept-relation
;;                   :source (concept-uuid concept2)
;;                   :target (concept-uuid concept1))))

;; (defun remove-child (child concept)
;;   "Remove relationship between CHILD and CONCEPT if CHILD is a child of
;; CONCEPT."
;;   (when (childp child concept)
;;     (unlink concept child)))

;; (defun remove-friend (concept1 concept2)
;;   "Remove friendship between CONCEPT1 and CONCEPT2 if they are friends."
;;   (when (friendp concept1 concept2)
;;     (unlink concept1 concept2)))

