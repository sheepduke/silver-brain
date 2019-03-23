(defpackage #:silver-brain.node-map
  (:nicknames #:node-map)
  (:use #:cl
        #:closer-mop)
  (:export :link-to
           :link-between))


(defun link-to (parent child)
  "Make a one-way link from `parent` to `child`."
  (push (uuid child) (children parent))
  (push (uuid parent) (parents child)))

(defun link-between (node1 node2)
  "Make a both-way link between `concept1` and `concept2`."
  (link-to node1 node2)
  (link-to node2 node1))

