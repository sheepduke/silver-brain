(in-package #:silver-brain.concept-map)

(with-auto-export ()
  (defclass concept ()
    ((uuid :type string
           :initarg :uuid
           :accessor uuid)
     (name :type string
           :initarg :name
           :accessor name)
     (aliases :type list
              :initarg :aliases
              :accessor aliases)
     (attachments :type list
                  :initarg attachments
                  :accessor attachments)
     (created-at :type time:timestamp
                 :initarg :created-at
                 :accessor created-at)
     (updated-at :type time:timestamp
                 :initarg :updated-at
                 :accessor updated-at))
    (:documentation "ALIASES is a list of strings.
LINKS is a list of `concept-link'.
LINKED-CONCEPTS is a list of `concept'.
ATTACHMENTS is a list of `concept-attachment'."))

  (defclass concept-attachment ()
    ((id :type string
         :initarg :id
         :accessor id)
     (name :type string
           :initarg :name
           :accessor name)
     (concept-uuid :type string
                   :initarg :concept-uuid
                   :accessor concept-uuid)
     (content-type :type string
                   :initarg :content-type
                   :accessor content-type)
     (content-length :type string
                     :initarg :content-length
                     :accessor content-length)))

  (defclass concept-links ()
    ((links :type list
            :initarg :links
            :accessor links
            :initform '())
     (concepts :type list
               :initarg :concepts
               :accessor concepts
               :initform '())))

  (defclass concept-link ()
    ((id :type string
         :initarg :id
         :accessor id)
     (source :type string
             :initarg :source
             :accessor source)
     (relation :type string
               :initarg :relation
               :accessor relation)
     (target :type string
             :initarg :target
             :accessor target)))

  (deftype concept-link-list ()
    `(and list (satisfies concept-link-list?)))

  (defcondition uuid-not-found (error)
      ((uuid :type string
             :initarg :uuid
             :accessor uuid))
    (:report (lambda (err stream)
               (format stream "Concept UUID '~A' is not found" (uuid err)))))

  (defun signal-uuid-not-found-error (uuid)
    (error 'uuid-not-found :uuid uuid)))

(defun concept-link-list? (links)
  (list:every? links (op (type? _ 'concept-link))))
