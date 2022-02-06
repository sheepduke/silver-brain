(defpackage silver-brain.util
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:~>>)
  (:export #:to-json-object
           #:is-uuid
           #:string-list
           #:client-error
           #:not-found-error
           #:bad-request-error))

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

(defgeneric to-json-object (obj))

(defmethod to-json-object ((obj standard-object))
  (let ((slot-names (~>> (class-of obj)
                         (c2mop:class-direct-slots)
                         (remove-if-not #'c2mop:slot-definition-readers)
                         (mapcar #'c2mop:slot-definition-name)))
        (js (jsown:new-js)))
    (dolist (slot-name slot-names)
      (when (slot-boundp obj slot-name)
        (let ((value (slot-value obj slot-name)))
          (setf js
                (jsown:extend-js js
                  ((str:downcase slot-name)
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
