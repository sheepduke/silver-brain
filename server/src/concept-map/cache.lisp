(defpackage silver-brain.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:store #:silver-brain.store))
  (:import-from #:serapeum
                #:op
                #:->)
  (:import-from #:trivia
                #:match)
  (:export #:start
           #:stop
           #:get-concept-name
           #:invalidate-if-exists))

(in-package silver-brain.concept-map.cache)

(defvar *cache* nil)

(defun make-cache ()
  (agt:make-agent (op (make-hash-table :test #'equal))))

(defun start ()
  (unless *cache*
    (setf *cache* (agt:make-agent (op (make-hash-table :test #'equal))))))

(defun stop ()
  (when *cache*
    (agt:agent-stop *cache*)
    (setf *cache* nil)))

(defun get-concept-name (uuid)
  (match (or (agt:agent-get *cache* (op (gethash uuid _)))
             (store:get 'store:concept uuid))
    ;; Directly return when in cache.
    ((and (type string) name)
     name)
    ;; Store to cache when database hit.
    ((store:concept :name name)
     (agt:agent-update *cache*
                       (op (setf (gethash uuid _) name) _1))
     name)
    (_ nil)))

(defun invalidate-if-exists (uuid)
  "Invalidate the cache entry if exists."
  (agt:agent-update *cache* (op (remhash uuid _1) _1)))
