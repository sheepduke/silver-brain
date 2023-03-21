(in-package #:silver-brain.concept-map)

(with-auto-export ()
  (defun load-concept (uuid &key load-aliases? load-attachments? load-times?
                               (link-level 0) load-linked-aliases?
                               load-linked-attachments? load-linked-times?)
    (match (load-concept* uuid load-aliases? load-attachments? load-times?)
      (nil nil)
      (concept
       (when (> link-level 0)
         (setf (links concept)
               (load-concept-links* uuid link-level))

         (setf (linked-concepts concept)
               (pipe (links concept)
                     (links-source-target-uuids)
                     (list:map (fun (uuid)
                                 (cons uuid
                                       (load-concept* uuid
                                                      load-linked-aliases?
                                                      load-linked-attachments?
                                                      load-linked-times?)))))))
       concept))))

(defun load-concept* (uuid load-aliases? load-attachments? load-times?)
  (let* ((dao (mito:find-dao 'store:concept :uuid uuid))
         (concept (make-instance 'concept
                                 :uuid (store:uuid dao)
                                 :name (store:name dao))))
    (when load-aliases?
      (setf (aliases concept) (load-aliases uuid)))

    (when load-attachments?
      (setf (attachments concept) (load-attachments* uuid)))

    (when load-times?
      (setf (created-at concept) (mito:object-created-at dao))
      (setf (updated-at concept) (mito:object-updated-at dao)))

    concept))

(defun load-aliases (uuid)
  (pipe (mito:select-dao 'store:concept-alias
          (sxql:where (:= :concept-uuid uuid)))
        (list:map #'store:alias)))

(defun load-attachments* (uuid)
  (pipe (mito:select-dao 'store:concept-attachment
          (sxql:where (:= :concept-uuid uuid)))
        (list:map (fun (dao)
                    (make-instance 'concept-attachment
                                   :id (mito:object-id dao)
                                   :concept-uuid (store:concept-uuid dao)
                                   :name (store:name dao)
                                   :content-type (store:content-type dao)
                                   :content-length (store:content-length dao))))))

(-> load-concept-links* (string integer) (concept-link-list))
(defun load-concept-links* (uuid link-level)
  (local
    (defun dao->link (dao)
      (make-instance 'concept-link
                     :id (mito:object-id dao)
                     :source (store:source-uuid dao)
                     :relation (store:relation-uuid dao)
                     :target (store:target-uuid dao)))

    (defun aux (link-level links uuids processed)
      (if (<= link-level 0)
          links
          (let* ((daos (mito:select-dao 'store:concept-link
                         (sxql:where (:and (:or (:in :target-uuid uuids)
                                                (:in :source-uuid uuids))
                                           (:not (:in :source-uuid processed))
                                           (:not (:in :target-uuid processed))))))
                 (new-links (list:map daos #'dao->link)))
            (aux (1- link-level)
                 (list:concat links new-links)
                 (pipe (links-source-target-uuids new-links)
                       (lset:difference processed))
                 (pipe (list:concat processed uuids))))))

    (aux link-level '() (list uuid) '())))

(defun links-source-target-uuids (links)
  (pipe (list:map links (fun (link) (list (source link) (target link))))
        (list:flatten)
        (list:remove-duplicates)))
