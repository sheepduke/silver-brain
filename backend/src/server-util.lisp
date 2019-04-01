(defpackage silver-brain.server-util
  (:nicknames :util)
  (:use :cl :alexandria)
  (:import-from #:caveman2
                #:*request*
                #:*response*
                #:response-status
                #:response-headers)
  (:import-from #:cl-json
                #:decode-json-from-string
                #:encode-json-to-string)
  (:import-from #:trivial-types
                #:association-list-p)
  (:export #:set-response-status
           #:set-response-header
           #:decode-request-json-alist))
(in-package silver-brain.server-util)

(defun request-body ()
  "Extract and return raw request body as string."
  (flexi-streams:octets-to-string
   (lack.request:request-content *request*) :external-format :utf-8))

(defun set-response-status (status-code)
  "Set response status to `status-code`."
  (setf (response-status *response*) (format nil "~a" status-code)))

(defun set-response-header (header value)
  "Set `header` of current response to `value`."
  (setf (getf (response-headers *response*) header) value))

  (defun decode-request-json-alist (keys &key (strict t))
    "Decode request body `json` as JSON string to associate list.
Argument `keys` is a list of keywords specifying keys to extract from the
decoded list.
Returns a list of values associated to `keys`.
If `strict` is set to `T`, return `NIL` when any key is not present."
    (let ((obj (decode-json-from-string (request-body))))
      (cond
        ((not (association-list-p obj)) nil)
        (t
         (let ((result (mapcar (lambda (key) (assoc-value obj key)) keys)))
           (if (and strict (some #'null result))
               nil
               result))))))
