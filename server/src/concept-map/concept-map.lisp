(defpackage silver-brain.concept-map
  (:use #:cl
        #:silver-brain.util
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.concept-map.store))
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
           #:delete-concept))

(in-package silver-brain.concept-map)

(defun start ()
  (silver-brain.concept-map.cache:start))

(defun stop ()
  (silver-brain.concept-map.cache:stop))

(-> create-database (string) service-response)
(defun create-database (name)
  (store:create-database name)
  (make-ok-response))

(-> get-concept (string) service-response)
(defun get-concept (uuid)
  (if (not (is-uuid uuid))
      (make-bad-request-response "Invalid UUID")
      (if-let (concept (store:get-concept-by-uuid uuid))
        (make-ok-response concept)
        (make-not-found-response))))

(-> search-concept (string) service-response)
(defun search-concept (search)
  (make-ok-response 
   (store:search-concept-by-string (~>> (str:split " " search)
                                        (remove-if #'str:emptyp)))))

(-> create-concept (&key (:name string)
                         (:content-type string)
                         (:content string))
  string)
(defun create-concept (&key name content-type content)
  (store:create-concept :name name
                        :content-type content-type
                        :content content))

(defun update-concept (uuid &key name content-type content)
  (match (get-concept uuid)
    ((list :ok _)
     (store:update-concept uuid
                           :name name
                           :content-type content-type
                           :content content))
    (else else)))

(-> delete-concept (string) service-response)
(defun delete-concept (uuid)
  (if (store:used-as-link-p uuid)
      '(:error :conflict "Concept used as link")
      (progn (store:delete-concept uuid)
             '(:ok))))

;; (silver-brain:start)

;; (dex:get (format nil "http://localhost:5001/api/concept/~a" "5BAAB06F-D70D-4405-8511-3032D12448B3") :headers '(("database" . "a.sqlite")))

;; (dex:get (format nil "http://localhost:5001/api/concept/8FB1298A-D5B0-4B7E-9099-B6E2A77C84A5") :headers '(("database" . "a.sqlite")))
