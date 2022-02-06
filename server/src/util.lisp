(defpackage silver-brain.util
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let)
  (:import-from #:serapeum
                #:->
                #:~>>)
  (:export #:to-json-object
           #:is-uuid
           #:string-list
           #:client-error
           #:not-found-error
           #:bad-request-error
           #:json-serializable-class))

(in-package silver-brain.util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Types                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-string-p (list)
  (every #'stringp list))

(deftype string-list ()
  `(and list (satisfies every-string-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Conditions                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition client-error (error)
  ((reason :type string :accessor reason :initarg :reason)))

(define-condition not-found-error (client-error) ()
  (:report (lambda (err stream)
             (format stream "Resource not found error: ~a"
                     (reason err)))))

(define-condition bad-request-error (client-error) ()
  (:report (lambda (err stream)
             (format stream "Bad request error: ~a"
                     (reason err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         JSON Methods                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass json-serializable-class (standard-class) ())

(defclass json-serializable-slot-class (c2mop:standard-direct-slot-definition
                                        c2mop:standard-effective-slot-definition)
  ((json-key-name :type (or null string)
                  :accessor json-key-name
                  :initarg :json-key-name
                  :initform nil)))

(defmethod c2mop:direct-slot-definition-class ((class json-serializable-class)
                                               &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-serializable-slot-class))

(defmethod c2mop:effective-slot-definition-class ((class json-serializable-class)
                                                  &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-serializable-slot-class))

(defmethod c2mop:validate-superclass ((class json-serializable-class)
                                      (superclass standard-class))
  t)

(defmethod json-key-name (object)
  nil)

(defgeneric to-json-object (obj))

(defmethod to-json-object ((obj standard-object))
  (let ((slot-names-pairs
         (~>> (class-of obj)
              (c2mop:class-direct-slots)
              (remove-if-not #'c2mop:slot-definition-readers)
              (mapcar (lambda (slot)
                        (let ((slot-name (c2mop:slot-definition-name slot)))
                          (cons slot-name
                                (if-let (custom-name (json-key-name slot))
                                  custom-name
                                  slot-name)))))))
        (js (jsown:new-js)))
    (dolist (slot-name-pair slot-names-pairs)
      (let ((slot-name (car slot-name-pair))
            (json-key-name (cdr slot-name-pair)))
        (when (slot-boundp obj slot-name)
          (let ((value (slot-value obj slot-name)))
            (jsown:extend-js js ((str:downcase json-key-name)
                                 (if (typep value 'standard-object)
                                     (to-json-object value)
                                     value)))))))
    js))

(defmethod to-json-object ((obj local-time:timestamp))
  (local-time:to-rfc3339-timestring obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Service                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +uuid-scanner+
  (ppcre:create-scanner "^[a-z0-9]{8}-([a-z0-9]{4}-){3}[a-z0-9]{12}$"))

(-> is-uuid (string) (boolean))
(defun is-uuid (string)
  "Return T if given STRING is a valid UUID."
  (trivia:match (multiple-value-list (ppcre:scan +uuid-scanner+ string))
    (nil nil)
    ((list* 0 36 _) t)))
