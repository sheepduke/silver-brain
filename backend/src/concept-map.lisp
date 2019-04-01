(defpackage silver-brain.concept-map
  (:nicknames #:concept-map #:map)
  (:use #:cl
        #:iterate)
  (:export #:concept-map
           #:concepts
           #:concept-names
           #:add-concept
           #:get-by-id
           #:map-concept))
(in-package :silver-brain.concept-map)

(defclass concept-map ()
  ((concepts :accessor concepts
             :type hash-table
             :initform (make-hash-table :test #'equal)
             :documentation "A hash table from ID to concepts."))
  (:documentation "A concept map consists of concepts."))

(defun add-concept (map concept)
  "Add `concept` into `map`."
  (setf (gethash (concept:id concept) (concepts map)) concept))

(defun get-by-id (map id)
  "Return corresponding `concept` from `map` with given `id`."
  (gethash id (concepts map)))

(defun map-concept (concept-map fun)
  "Works like `mapcar`. It traverses all concepts and apply `fun` to each
concept and returns a list.
Argument `fun` is a function taking one argument as the concept."
  (iter (for (key value) in-hashtable (concepts concept-map))
    (collect (funcall fun value))))
