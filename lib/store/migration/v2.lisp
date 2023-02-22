(unlisp.prelude:defpackage #:silver-brain.store.migration.v2
  (:use #:unlisp.prelude
        #:silver-brain.store.migration.util)
  (:local-nicknames (#:v2 #:silver-brain.store.schema.v2))
  (:import-from #:mito.dao.mixin
                #:created-at
                #:updated-at))

(in-package #:silver-brain.store.migration.v2)

(unlisp.dev:setup-package-local-nicknames)

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

(with-auto-export ()
  (defun run ()
    (when (and (not (table-exists? "meta_info"))
               (table-exists? "concept")
               (table-exists? "concept_"))

      (rename-legacy-tables)
      (create-new-tables)
      (migrate-legacy-data)
      (drop-legacy-tables))))

(defun rename-legacy-tables ()
  (mito:execute-sql "alter table concept rename to legacy_concept")
  (mito:execute-sql "alter table concept_relation rename to legacy_relation"))

(defun create-new-tables ()
  (let ((tables '(v2:meta-info v2:concept v2:concept-alias v2:concept-attachment
                  v2:concept-pair v2:concept-link)))
    (list:doeach (table tables)
      (mito:ensure-table-exists table))))

(defun migrate-legacy-data ()
  (migrate-legacy-concepts)
  (migrate-legacy-relations))

(defun migrate-legacy-concepts ()
  (list:doeach (concept (mito:select-dao 'legacy-concept))
    (ematch concept
      ((legacy-concept uuid name content content-format created-at updated-at)
       (mito:create-dao 'v2:concept
                        :uuid uuid
                        :name name
                        :created-at created-at
                        :updated-at updated-at)
       (mito:create-dao 'v2:concept-attachment
                        :uuid uuid
                        :content-type content-format
                        :content content
                        :hyperlink? nil
                        :created-at created-at
                        :updated-at updated-at)))))

(defun migrate-legacy-relations ()
  (let (;; Create necessary relations.
        (parent-relation (mito:create-dao 'v2:concept :name "Is parent of"))
        (child-relation (mito:create-dao 'v2:concept :name "Is child of"))
        (friend-relation (mito:create-dao 'v2:concept :name "Relates to")))
    
    ;; Make parent and child a pair.
    (mito:create-dao 'v2:concept-pair
                     :uuid (v2:uuid parent-relation)
                     :other (v2:uuid child-relation))
    ;; Make friend and friend a pair (self pair).
    (mito:create-dao 'v2:concept-pair
                     :uuid (v2:uuid friend-relation)
                     :other (v2:uuid friend-relation))

    ;; For each legacy relation, turn it into a link.
    (list:doeach (legacy-relation (mito:select-dao 'legacy-relation))
      (ematch legacy-relation
        ((legacy-relation source target created-at updated-at)
         (when (and (valid-legacy-uuid? source)
                    (valid-legacy-uuid? target))
           (let ((relation (if (friend? source target)
                               friend-relation
                               parent-relation)))
             (mito:create-dao 'v2:concept-link
                              :source source
                              :relation relation
                              :target target
                              :created-at created-at
                              :updated-at updated-at))))))

    ;; Remove duplicated friend links.
    (let ((processed (htbl:make)))
      (list:doeach (link (mito:select-dao 'v2:concept-link
                           (sxql:where (:= :relation friend-relation))))
        (ematch link
          ((v2:concept-link :source source :target target)
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

;; (mito:connect-toplevel :sqlite3
;;                        :database-name (path:join (path:user-home)
;;                                                  "temp/silver-brain/v1.sqlite"))

;; (mito:create-dao 'silver-brain.store.schema.v1:concept
;;                  :uuid "11"
;;                  :name "AA"
;;                  :content "Content"
;;                  :content-format "text/org")

;; (mito:create-dao 'silver-brain.store.schema.v1:concept
;;                  :uuid "22"
;;                  :name "BB")

;; (mito:create-dao 'silver-brain.store.schema.v1:concept
;;                  :uuid "33"
;;                  :name "CC"
;;                  :content-format "text/md")

;; (mito:create-dao 'silver-brain.store.schema.v1:concept-relation
;;                  :source "11"
;;                  :target "22")

;; (mito:create-dao 'silver-brain.store.schema.v1:concept-relation
;;                  :source "11"
;;                  :target "33")

;; (mito:create-dao 'silver-brain.store.schema.v1:concept-relation
;;                  :source "33"
;;                  :target "11")

;; (mito:select-dao 'v2:concept)
;; (mito:select-dao 'v2:concept-pair)
