(defpackage silver-brain.concept-map
  (:nicknames #:concept-map #:map)
  (:use #:cl)
  (:export #:concept-map
           #:concepts
           #:concept-names
           #:add-concept
           #:get-by-id))
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
