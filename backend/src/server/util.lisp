(in-package silver-brain.server)

(defvar *app* (make-instance 'ningle:app))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Exceptions                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition not-found (error) ())
(define-condition bad-request (error) ())

(defun concept-summary (concept)
  "Return an alist representing summary information of given `concept`."
  `((:uuid . ,(concept-uuid concept))
    (:name . ,(concept-name concept))))

(defun get-concept-by-uuid-or-404 (uuid)
  "Return concept instance from given UUID. If the UUID is invalid, throws "
  (let ((concept (service:get-concept-by-uuid uuid)))
    (or concept
        (error 'not-found))
    concept))

(defmacro defroute (app http-method url-rule var-list &body body)
  "Define route for given APP.
HTTP-METHOD is one of GET, POST, PUT and DELETE.
URL-RULE is the rule that will be passed to route definition.
VAR-LIST is a list of variables that will be interpreted from path variable or
request parameter."
  (check-type var-list list "lambda list")
  (with-gensyms (g-params)
    `(setf (ningle:route ,app ,url-rule :method ,(make-keyword http-method))
           (lambda (,g-params)
             (declare (ignorable ,g-params))
             (let ,(mapcar (lambda (var)
                             `(,var (get-param ',var ,g-params)))
                    var-list)
               (handler-case (progn ,@body)
                 (not-found ()
                   (set-response-status 404))
                 (bad-request ()
                   (set-response-status 400))))))))

(defun get-param (key params)
  "Return parameter denoted by KEY from PARAMS."
  (assoc-value params key :test #'string-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Request & Response                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun request-body ()
  "Extract and return raw request body as string."
  (flexi-streams:octets-to-string
   (lack.request:request-content *request*)
   :external-format :utf-8))

(defun set-response-status (status-code)
  "Set response status to `status-code`."
  (setf (response-status *response*)
        (format nil "~a" status-code)))

(defun set-response-header (header value)
  "Set `header` of current response to `value`."
  (setf (getf (response-headers *response*) header) value))

(defun set-response-location (value)
  "Set location header to given `value`."
  (set-response-header "Location" value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             JSON                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-json (thing)
  "Encode given `thing` to JSON string and set `Content-Type` header.
Return corresponding JSON."
  (set-response-header "Content-Type" "application/json")
  (json:encode-json-to-string thing))

(defun render-json-array (thing)
  "Works like `render-json`. When `thing` is empty, return an empty array."
  (render-json (if thing thing '#())))

(defun decode-request-json-alist (keys &key (strict t))
  "Decode request body JSON as JSON string to associate list.
Argument KEYS is a list of keywords specifying keys to extract from the
decoded list.
Returns a list of values associated to KEYS.
If STRICT is set to T, throws BAD-REQUEST error."
  (let ((obj (handler-case (json:decode-json-from-string (request-body))
               (error () nil))))
    (cond
      ((not (association-list-p obj)) nil)
      (t
       (let ((result (mapcar (lambda (key) (assoc-value obj key)) keys)))
         (when (and strict (some #'null result))
           (error 'bad-request))
         result)))))
