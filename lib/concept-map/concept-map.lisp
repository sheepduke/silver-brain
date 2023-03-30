(in-package #:silver-brain.concept-map)

(with-auto-export ()
  (-> list-concepts
      (string &key (:load-aliases? boolean) (:load-times? boolean))
      concept-list)
  (defun list-concepts (search &key (load-aliases? t) load-times?)
    (pipe (search-concepts search)
          (list:map (fun (dao)
                      (let ((concept (make-instance 'concept
                                                    :uuid (store:uuid dao)
                                                    :name (store:name dao))))
                        (when load-aliases?
                          (setf (aliases concept)
                                (get-aliases (store:uuid dao))))

                        (when load-times?
                          (setf (created-at concept) (mito:object-created-at dao))
                          (setf (updated-at concept) (mito:object-updated-at dao)))

                        concept)))))

  (-> get-concept (string &key (:load-aliases? boolean)
                          (:load-attachments? boolean)
                          (:load-times? boolean))
      (nullable concept))
  (defun get-concept (uuid &key load-aliases? load-attachments? load-times?)
    (let* ((dao (mito:find-dao 'store:concept :uuid uuid)))
      (match (mito:find-dao 'store:concept :uuid uuid)
        (nil (error 'global:not-found-error))
        (dao
         (let ((concept (make-instance 'concept
                                       :uuid (store:uuid dao)
                                       :name (store:name dao))))
           (when load-aliases?
             (setf (aliases concept) (get-aliases uuid)))

           (when load-attachments?
             (setf (attachments concept) (get-attachments* uuid)))

           (when load-times?
             (setf (created-at concept) (mito:object-created-at dao))
             (setf (updated-at concept) (mito:object-updated-at dao)))

           concept)))))

  (-> get-concept-links (string &key (:link-level integer)
                                (:load-aliases? boolean)
                                (:load-attachments? boolean)
                                (:load-times? boolean))
      concept-links)
  (defun get-concept-links (uuid &key (link-level 1) load-aliases?
                                   load-attachments? load-times?)
    (assert (>= link-level 0) (link-level)
            "LINK-LEVEL must not be negative.")

    (let* ((links (get-concept-links* uuid link-level))
           (concepts (pipe links 
                           (links-source-target-uuids)
                           (list:filter (op (string:/= _ uuid)))
                           (list:map
                            (fun (uuid)
                              (cons uuid
                                    (get-concept
                                     uuid
                                     :load-aliases? load-aliases?
                                     :load-attachments? load-attachments?
                                     :load-times? load-times?)))))))
      (make-instance 'concept-links
                     :links links
                     :concepts concepts))))

(defun get-aliases (uuid)
  (pipe (mito:select-dao 'store:concept-alias
          (sxql:where (:= :concept-uuid uuid)))
        (list:map #'store:alias)))

(defun get-attachments* (uuid)
  (pipe (mito:select-dao 'store:concept-attachment
          (sxql:where (:= :concept-uuid uuid)))
        (list:map (fun (dao)
                    (make-instance 'concept-attachment
                                   :id (mito:object-id dao)
                                   :concept-uuid (store:concept-uuid dao)
                                   :name (store:name dao)
                                   :content-type (store:content-type dao)
                                   :content-length (store:content-length dao))))))

(-> get-concept-links* (string integer) (concept-link-list))
(defun get-concept-links* (uuid link-level)
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
