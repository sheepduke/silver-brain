(in-package #:silver-brain.concept-map)

(with-auto-export ()
  (defclass concept ()
    ((uuid :initarg :uuid
           :accessor uuid)
     (name :initarg :name
           :accessor name)
     (aliases :initarg :aliases
              :accessor aliases)
     (attachments :initarg attachments
                  :accessor attachments)
     (created-at :initarg :created-at
                 :accessor created-at)
     (updated-at :initarg :updated-at
                 :accessor updated-at))
    (:documentation "ALIASES is a list of strings.
LINKS is a list of `concept-link'.
LINKED-CONCEPTS is a list of `concept'.
ATTACHMENTS is a list of `concept-attachment'."))

  (defclass concept-attachment ()
    ((id :initarg :id
         :accessor id)
     (name :initarg :name
           :accessor name)
     (concept-uuid :initarg :concept-uuid
                   :accessor concept-uuid)
     (content-type :initarg :content-type
                   :accessor content-type)
     (content-length :initarg :content-length
                     :accessor content-length)))

  (defclass concept-links ()
    ((links :initarg :links
            :accessor links
            :initform '())
     (concepts :initarg :concepts
               :accessor concepts
               :initform '())))

  (defclass concept-link ()
    ((id :initarg :id
         :accessor id)
     (source :initarg :source
             :accessor source)
     (relation :initarg :relation
               :accessor relation)
     (target :initarg :target
             :accessor target)))

  (deftype concept-link-list ()
    `(and list (satisfies concept-link-list?)))

  (defcondition uuid-not-found (error)
      ((uuid :initarg :uuid
             :accessor uuid))
    (:report (lambda (err stream)
               (format stream "Concept UUID '~A' is not found" (uuid err)))))

  (defun signal-uuid-not-found-error (uuid)
    (error 'uuid-not-found :uuid uuid)))

(defun concept-link-list? (links)
  (list:every? links (op (type? _ 'concept-link))))
