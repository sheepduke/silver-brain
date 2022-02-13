;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'silver-brain-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Basic HTTP                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--api-body-string ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring (point) (point-max))))

(defun silver-brain--api-status-code ()
  "Extract the HTTP status code in number format from response."
  (save-excursion
    (goto-char (point-min))
    (search-forward "HTTP/")
    (let ((end (search-forward-regexp "[0-9]\\{3\\}")))
      (car (read-from-string (buffer-substring (- end 3) end))))))

(cl-defun silver-brain--api-send-request (uri &key (method :get) data)
  "Send HTTP request to URI with Database header set. Return the response buffer."
  (let ((url-request-extra-headers `(("Database" . ,silver-brain-database-name)))
        (url-request-method (cl-case method
                              (:get "GET")
                              (:post "POST")
                              (:patch "PATCH")
                              (:delete "DELETE")))
        (url-request-data (and data (encode-coding-string (json-encode data) 'utf-8))))
    (let ((buffer (url-retrieve-synchronously (format "http://localhost:%d/api/%s"
                                                      silver-brain-server-port
                                                      uri))))
      (with-current-buffer buffer
        (let ((code (silver-brain--api-status-code)))
          (unless (<= 200 code 299)
            (error (format "Server response %d: %s"
                           code
                           (string-trim (silver-brain--api-body-string)))))))
      buffer)))

(cl-defun silver-brain--api-read-json (&key (object-type 'alist)
                            (key-type 'string))
  "Read response body as JSON and parse it.
OBJECT-TYPE and KEY-TYPE is set to JSON-KEY-TYPE and JSON-ARRAY-TYPE."
  (let ((json-object-type object-type)
        (json-key-type key-type)
        (json-array-type 'list))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n\n")
      (json-read))))

(defun silver-brain--api-get (uri)
  (with-current-buffer (silver-brain--api-send-request uri :method :get)
    (silver-brain--api-read-json)))

(defun silver-brain--api-post (uri data)
  (with-current-buffer (silver-brain--api-send-request uri
                                       :method :post
                                       :data data)
    (silver-brain--api-read-json)))

(defun silver-brain--api-patch (uri data)
  (silver-brain--api-send-request uri
                  :method :patch
                  :data data))

(defun silver-brain--api-delete (uri)
  (silver-brain--api-send-request uri :method :delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Conversion                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--api-assoc (alist keys)
  (cond
   ((atom keys) (alist-get keys alist nil nil #'string-equal))
   ((null (cdr keys)) (alist-get (car keys) alist nil nil #'string-equal))
   (t (silver-brain--api-assoc alist (cdr keys)))))

(defun silver-brain--api-alist->concept (alist)
  (make-silver-brain-concept
   :uuid (silver-brain--api-assoc alist "uuid")
   :name (silver-brain--api-assoc alist "name")
   :content-type (silver-brain--api-assoc alist "contentType")
   :content (silver-brain--api-assoc alist "content")
   :links (mapcar #'silver-brain--api-alist->concept-link (silver-brain--api-assoc alist "links"))
   :create-time (silver-brain--api-assoc alist "createTime")
   :update-time (silver-brain--api-assoc alist "updateTime")))

(defun silver-brain--api-alist->concept-link (alist)
  (make-silver-brain-concept-link
   :uuid (silver-brain--api-assoc alist "uuid")
   :source (silver-brain--api-alist->concept-summary (silver-brain--api-assoc alist "source"))
   :relation (silver-brain--api-alist->concept-summary (silver-brain--api-assoc alist "relation"))
   :target (silver-brain--api-alist->concept-summary (silver-brain--api-assoc alist "target"))
   :directionalp (silver-brain--api-assoc alist "isDirectional")))

(defun silver-brain--api-alist->concept-summary (alist)
  (make-silver-brain-concept-summary
   :uuid (silver-brain--api-assoc alist "uuid")
   :name (silver-brain--api-assoc alist "name")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Data Model                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct silver-brain-concept
  uuid name content-type content links create-time update-time)

(cl-defstruct silver-brain-concept-summary uuid name)

(defun silver-brain-concept-summary-by-uuid-< (a b)
  (string< (silver-brain-concept-summary-uuid a)
           (silver-brain-concept-summary-uuid b)))

(defun silver-brain-concept-summary-by-name-< (a b)
  (string< (silver-brain-concept-summary-name a)
           (silver-brain-concept-summary-name b)))

(cl-defstruct silver-brain-concept-link
  uuid source relation target directionalp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             API                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-api-get-concept (uuid)
  (silver-brain--api-alist->concept (silver-brain--api-get (format "concepts/%s" uuid))))

(defun silver-brain-api-search-concept (search-string)
  (mapcar #'silver-brain--api-alist->concept-summary
          (silver-brain--api-get (format "concepts?search=%s" search-string))))

(defun silver-brain-api-create-concept (name content-type)
  (silver-brain--api-alist->concept (silver-brain--api-post "concepts"
                            `(("name" . ,name)
                              ("contentType" . ,content-type)))))

(cl-defun silver-brain-api-update-concept (uuid &key name content-type content)
  (let ((data '()))
    (when name
      (push (cons "name" name)))
    (when content-type
      (push (cons "content-type" content-type) data))
    (when content
      (push (cons "content" content) data))
    (silver-brain--api-patch (format "concepts/%s" uuid) data)))

(defun silver-brain-api-delete-concept (uuid)
  (silver-brain--api-delete (format "concepts/%s" uuid)))

(defun silver-brain-api-create-link (source relation target directionalp)
  (silver-brain--api-post "concept-links"
          `(("source" . ,source)
            ("relation" . ,relation)
            ("target" . ,target)
            ("isDirectional" . ,directionalp))))

(provide 'silver-brain-api)
