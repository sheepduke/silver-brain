(defpackage silver-brain.concept-map
  (:use #:cl
        #:silver-brain.util
        #:silver-brain.concept-map.model)
  (:local-nicknames (#:store #:silver-brain.concept-map.store))
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:serapeum
                #:->
                #:defsubst)
  (:export #:start
           #:stop
           #:get-concept
           #:search-concept))

(in-package silver-brain.concept-map)

(defun start ()
  (silver-brain.concept-map.cache:start))

(defun stop ()
  (silver-brain.concept-map.cache:stop))

(-> get-concept (string) service-response)
(defun get-concept (uuid)
  (if (not (is-uuid uuid))
      (make-bad-request-response :invalid-uuid)
      (if-let (concept (store:get-concept-by-uuid uuid))
        (make-ok-response concept)
        (make-not-found-response))))

(defun search-concept (search)
  (make-ok-response 
   (store:search-concept-by-string search)))

;; (silver-brain:start)

;; (dex:get (format nil "http://localhost:5001/api/concepts/~a" "5BAAB06F-D70D-4405-8511-3032D12448B3") :headers '( ("database" . "/home/sheep/temp/a.sqlite")))

;; (ql:quickload :dexador)

;; TODO Re-design auto migration
;; Add a keyword argument to with-database.
;; Run migration when startup.
;; Create database: auto-create & auto-migrate
