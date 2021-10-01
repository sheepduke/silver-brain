(defpackage silver-brain.concept-map.cache
  (:use #:cl)
  (:local-nicknames (#:globals #:silver-brain.globals))
  (:import-from #:serapeum
                #:->)
  (:import-from #:trivia
                #:match)
  (:export #:start
           #:get-concept-name))

(in-package silver-brain.concept-map.cache)

(defvar *cache-actor* nil)

(defun start (&key (init (fset:empty-map)))
  (unless *cache-actor*
    (setf *cache-actor*
          (act:actor-of (globals:*actor-system*)
            :receive (lambda (self msg state)
                       (declare (ignore self))
                       (match msg
                         ((list :get uuid) (handle-get-concept-name uuid state)))
                       (cons nil state))
            :init init))))

(defun get-concept-name (uuid)
  (act:ask-s *cache-actor* (list :get uuid)))

(-> handle-get-concept-name (string fset:map) (cons (or string null) fset:map))
(defun handle-get-concept-name (uuid cache)
  (let ((name (match (or (fset:lookup cache uuid)
                         (mito:find-dao 'store:concept :uuid uuid))
                ((string name) name)
                ((store:concept :name name) name)
                (_ nil))))
    (print name)
    (if name
        (cons name (fset:map (fset:$ cache) (uuid name)))
        (cons nil cache))))
