(unlisp.prelude:defpackage #:silver-brain.store.migration.v2
  (:use #:unlisp.prelude
        #:silver-brain.store.migration.util)
  (:local-nicknames (#:schema #:silver-brain.store.schema)
                    (#:v1 #:silver-brain.store.schema.v1)
                    (#:v2 #:silver-brain.store.schema.v2)
                    (#:migration.v1 #:silver-brain.store.migration.v1))
  (:import-from #:mito.dao.mixin
                #:created-at
                #:updated-at))

(in-package #:silver-brain.store.migration.v2)

(unlisp.dev:setup-package-local-nicknames)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                    Renamed Legacy Tables                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass legacy-concept ()
  ((uuid :col-type (:varchar 64)
         :initarg :uuid)
   (name :col-type (:varchar 1024)
         :initarg :name)
   (content :col-type (:varchar 1024)
            :initarg :content
            :initform "")
   (content-format :col-type (:varchar 16)
                   :initarg :content-format))
  (:metaclass mito:dao-table-class))

(defclass legacy-relation ()
    ((source :col-type (:varchar 64)
             :initarg :source)
     (target :col-type (:varchar 64)
             :initarg :target))
    (:metaclass mito:dao-table-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                       Migration Logic                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-auto-export ()
  (defun run ()
    (let ((current-data-version (fetch-data-version)))
      (cond 
        ;; For a new database, just create the new schema.
        ((string:= current-data-version schema:new-schema-version)
         (dbi:with-transaction mito:*connection*
           (create-new-tables)
           (update-data-version)))
        ;; For database with old data, migrate it.
        ((string:= current-data-version v1:schema-version)
         (dbi:with-transaction mito:*connection*
           (rename-legacy-tables)
           (create-new-tables)
           (migrate-legacy-data)
           (drop-legacy-tables)
           (update-data-version)))))))

(defun rename-legacy-tables ()
  (mito:execute-sql "alter table concept rename to legacy_concept")
  (mito:execute-sql "alter table concept_relation rename to legacy_relation"))

(defun create-new-tables ()
  (let ((tables '(v2:concept v2:concept-alias v2:concept-attachment
                  v2:concept-pair v2:concept-link)))
    (list:doeach (table tables)
      (mito:ensure-table-exists table))))

(defun migrate-legacy-data ()
  (migrate-legacy-concepts)
  (when (> (mito:count-dao 'v2:concept) 0)
    (migrate-legacy-relations)))

(defun migrate-legacy-concepts ()
  (list:doeach (concept (mito:select-dao 'legacy-concept))
    (ematch concept
      ((legacy-concept uuid name content content-format created-at updated-at)
       (mito:create-dao 'v2:concept
                        :uuid uuid
                        :name name
                        :created-at created-at
                        :updated-at updated-at)
       ;; If the concept has any content, create an attachment for it.
       (unless (string:empty? content)
         (mito:create-dao 'v2:concept-attachment
                          :concept-uuid uuid
                          :content-type content-format
                          :content content
                          :hyperlink? nil
                          :created-at created-at
                          :updated-at updated-at))))))

(defun migrate-legacy-relations ()
  (let (;; Create necessary relations.
        (parent (mito:create-dao 'v2:concept :name "Is parent of"))
        (child (mito:create-dao 'v2:concept :name "Is child of"))
        (friend (mito:create-dao 'v2:concept :name "Relates to")))

    ;; Make parent and child a pair.
    (mito:create-dao 'v2:concept-pair
                     :concept parent
                     :other child)
    ;; Make friend and friend a pair (self pair).
    (mito:create-dao 'v2:concept-pair
                     :concept friend
                     :other friend)

    ;; For each legacy relation, turn it into a link.
    (list:doeach (legacy-relation (mito:select-dao 'legacy-relation))
      (ematch legacy-relation
        ((legacy-relation source target created-at updated-at)
         (when (and (valid-legacy-uuid? source)
                    (valid-legacy-uuid? target))
           (let ((relation (if (friend? source target)
                               friend
                               parent)))
             (mito:create-dao 'v2:concept-link
                              :source-uuid source
                              :relation relation
                              :target-uuid target
                              :created-at created-at
                              :updated-at updated-at))))))

    ;; Remove duplicated friend links.
    (let ((processed (htbl:make)))
      (list:doeach (link (mito:select-dao 'v2:concept-link
                           (sxql:where (:= :relation friend))))
        (ematch link
          ((v2:concept-link :source-uuid source :target-uuid target)
           (if (htbl:contains? processed (cons target source))
               (mito:delete-dao link)
               (htbl:put processed (cons source target) t))))))))

(defun valid-legacy-uuid? (uuid)
  (>= (mito:count-dao 'legacy-concept :uuid uuid) 1))

(defun friend? (source target)
  (and (>= (mito:count-dao 'legacy-relation :source source :target target) 1)
       (>= (mito:count-dao 'legacy-relation :source target :target source) 1)))

(defun drop-legacy-tables ()
  (mito:execute-sql "drop table legacy_concept")
  (mito:execute-sql "drop table legacy_relation"))

(defun update-data-version ()
  (mito:update-dao (clone-object (mito:find-dao 'v2:meta-info)
                                 :data-version v2:schema-version)))
