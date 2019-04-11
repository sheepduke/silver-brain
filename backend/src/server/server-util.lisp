(in-package silver-brain)

(defun request-body ()
  "Extract and return raw request body as string."
  (flexi-streams:octets-to-string
   (lack.request:request-content caveman2:*request*)
   :external-format :utf-8))

(defun render-json (thing)
  "Encode given `thing` to JSON string and set `Content-Type` header.
Return corresponding JSON."
  (set-response-header "Content-Type" "application/json")
  (json:encode-json-to-string thing))

(defun render-json-array (thing)
  "Works like `render-json`. When `thing` is empty, return an empty array."
  (render-json (if thing thing '#())))

(defun set-response-status (status-code)
  "Set response status to `status-code`."
  (setf (caveman2:response-status caveman2:*response*) (format nil "~a" status-code)))

(defun set-response-header (header value)
  "Set `header` of current response to `value`."
  (setf (getf (caveman2:response-headers caveman2:*response*) header) value))

(defun set-response-location-header (value)
  "Set location header to given `value`."
  (set-response-header "Location" value))

(defun decode-request-json-alist (keys &key (strict t))
  "Decode request body `json` as JSON string to associate list.
Argument `keys` is a list of keywords specifying keys to extract from the
decoded list.
Returns a list of values associated to `keys`.
If `strict` is set to `T`, return `NIL` when any key is not present."
  (let ((obj (handler-case (json:decode-json-from-string (request-body))
               (error () nil))))
    (cond
      ((not (association-list-p obj)) nil)
      (t
       (let ((result (mapcar (lambda (key) (assoc-value obj key)) keys)))
         (if (and strict (some #'null result))
             nil
             result))))))

(defun concept-summary (concept)
  "Return an alist representing summary information of given `concept`."
  `((:uuid . ,(concept-uuid concept))
    (:name . ,(concept-name concept))))

