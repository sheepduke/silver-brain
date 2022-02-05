(defpackage silver-brain.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:store #:silver-brain.store))
  (:import-from #:serapeum
                #:op
                #:->)
  (:import-from #:trivia
                #:match)
  (:import-from #:alexandria
                #:when-let)
  (:export #:start
           #:stop
           #:get-concept-name
           #:invalidate-if-exists
           #:update-if-exists))

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

(defun update-if-exists (uuid name)
  (agt:agent-update *cache*
                    (lambda (hash-table)
                      (when (gethash uuid hash-table)
                        (setf (gethash uuid hash-table) name))
                      hash-table)))

(defun invalidate-if-exists (uuid)
  "Invalidate the cache entry if exists."
  (agt:agent-update *cache* (op (remhash uuid _1) _1)))
