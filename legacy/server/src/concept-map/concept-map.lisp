(defpackage silver-brain.concept-map
  (:use #:cl
        #:silver-brain.util
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:cm-store #:silver-brain.concept-map.store)
                    (#:store #:silver-brain.store))
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:serapeum
                #:~>>
                #:->
                #:defsubst)
  (:import-from #:trivia
                #:match)
  (:export #:start
           #:stop
           #:get-concept
           #:search-concept
           #:create-database
           #:create-concept
           #:update-concept
           #:delete-concept
           #:get-links
           #:create-link
           #:delete-links
           #:delete-link))

(in-package silver-brain.concept-map)

(defun start ())

(defun stop ())

(-> create-database (string) t)
(defun create-database (name)
  (cm-store:create-database name))

(-> get-concept (string) concept)
(defun get-concept (uuid)
  (store:with-current-database
    (store:with-transaction
        (validate-concept-uuid uuid)
      (cm-store:get-concept-by-uuid uuid))))

;; TODO: use properties argument instead of concept-summary
(-> search-concept (string) (values symbol concept-summary-list))
(defun search-concept (search)
  (store:with-current-database
    (cm-store:search-concept-by-string (~>> (str:split " " search)
                                            (remove-if #'str:emptyp)))))

(-> create-concept (&key (:name string)
                         (:content-type string)
                         (:content string))
  concept)
(defun create-concept (&key name content-type content)
  (store:with-current-database
    (cm-store:create-concept :name name
                             :content-type content-type
                             :content content)))

(-> update-concept (string &key (:name string)
                           (:content-type string)
                           (:content string))
  t)
(defun update-concept (uuid &key name content-type content)
  (store:with-current-database
    (store:with-transaction
        (validate-concept-uuid uuid)
      (cm-store:update-concept uuid
                               :name name
                               :content-type content-type
                               :content content))))

(-> delete-concept (string) t)
(defun delete-concept (uuid)
  (store:with-current-database
    (store:with-transaction
        (validate-concept-uuid uuid)
      (cm-store:delete-concept uuid))))

(-> create-link (string string string boolean) concept-link)
(defun create-link (source relation target directionalp)
  (store:with-current-database
    (store:with-transaction
        (validate-concept-uuid source 'bad-request-error)
      (validate-concept-uuid relation 'bad-request-error)
      (validate-concept-uuid target 'bad-request-error)
      (cm-store:create-link source relation target directionalp))))

(-> delete-link (string) t)
(defun delete-link (uuid)
  (store:with-current-database
    (store:with-transaction
        (validate-link-uuid uuid)
      (cm-store:delete-link uuid))))

(-> validate-concept-uuid (string &optional symbol) (values))
(defun validate-concept-uuid (uuid &optional (not-found-error 'not-found-error))
  (unless (is-uuid uuid)
    (error 'bad-request-error :reason (format nil "Invalid UUID '~a'" uuid)))
  (unless (cm-store:concept-uuid-exists-p uuid)
    (error not-found-error :reason (format nil "Concept UUID '~a'" uuid)))
  (values))

(defun validate-link-uuid (uuid)
  (unless (is-uuid uuid)
    (error 'bad-request-error :reason (format nil "Invalid UUID '~a'" uuid)))
  (unless (cm-store:link-uuid-exists-p uuid)
    (error 'not-found-error :reason (format nil "Link UUID '~a'" uuid)))
  (values))
