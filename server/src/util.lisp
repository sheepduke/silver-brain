(defpackage silver-brain.util
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:~>>
                #:defsubst)
  (:export #:to-json-object
           #:is-uuid
           #:service-response
           #:make-bad-request-response
           #:make-not-found-response
           #:make-ok-response))

(in-package silver-brain.util)

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

(deftype service-response ()
  'list)

(defsubst make-ok-response (&optional (thing nil thing-p))
  (if thing-p
      (list :ok thing)
      '(:ok)))

(defsubst make-bad-request-response (&optional (reason nil))
  (list :error :bad-request reason))

(defsubst make-not-found-response ()
  (list :error :not-found))

(defparameter +uuid-scanner+
  (ppcre:create-scanner "^[A-Z0-9]{8}-([A-Z0-9]{4}-){3}[A-Z0-9]{12}$"))

(-> is-uuid (string) (boolean))
(defun is-uuid (string)
  "Return T if given STRING is a valid UUID."
  (trivia:match (multiple-value-list (ppcre:scan +uuid-scanner+ string))
    (nil nil)
    ((list* 0 36 _) t)))
