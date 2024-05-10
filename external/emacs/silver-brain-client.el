;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'silver-brain-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Basic HTTP                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain--client-body-string ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring (point) (point-max))))

(defun silver-brain--client-status-code ()
  "Extract the HTTP status code in number format from response."
  (save-excursion
    (goto-char (point-min))
    (search-forward "HTTP/")
    (let ((end (search-forward-regexp "[0-9]\\{3\\}")))
      (car (read-from-string (buffer-substring (- end 3) end))))))

(cl-defun silver-brain--client-send-request (uri &key (method :get) data)
  "Send HTTP request to URI with Database header set. Return the response buffer."
  (let ((url-request-extra-headers `(("X-SB-Store" . ,silver-brain-store-name)))
        (url-request-method (cl-case method
                              (:get "GET")
                              (:post "POST")
                              (:patch "PATCH")
                              (:delete "DELETE")))
        (url-request-data (and data (encode-coding-string (json-encode data) 'utf-8))))
    (let ((buffer (url-retrieve-synchronously (format "http://localhost:%d/api/v2/%s"
                                                      silver-brain-server-port
                                                      uri))))
      (with-current-buffer buffer
        (let ((code (silver-brain--client-status-code)))
          (unless (<= 200 code 299)
            (error (format "Server response %d: %s"
                           code
                           (string-trim (silver-brain--client-body-string))))))

        (goto-char (point-min))
        (search-forward "\n\n")
        (decode-coding-string (buffer-substring (point) (point-max))
                              'utf-8)))))

(cl-defun silver-brain--client-read-json (string &key (object-type 'alist)
                                     (key-type 'string))
  "Read response body as JSON and parse it.
OBJECT-TYPE and KEY-TYPE is set to JSON-KEY-TYPE and JSON-ARRAY-TYPE."
  (let ((json-object-type object-type)
        (json-key-type key-type)
        (json-array-type 'list))
    (json-read-from-string string)))

(defun silver-brain--client-get (uri)
  (silver-brain--client-read-json (silver-brain--client-send-request uri :method :get)))

(defun silver-brain--client-post (uri &optional data)
  (silver-brain--client-read-json (silver-brain--client-send-request uri
                                             :method :post
                                             :data data)))

(defun silver-brain--client-patch (uri data)
  (silver-brain--client-send-request uri
                  :method :patch
                  :data data))

(defun silver-brain--client-delete (uri)
  (silver-brain--client-send-request uri :method :delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             API                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-client-get-item (id)
  (silver-brain--client-get (format "items/%s?props=all" id)))

(defun silver-brain-client-get-items (ids)
  (silver-brain--client-get (format "items?ids=%s&props=name"
                        (string-join ids ","))))

(defun silver-brain-client-search-items (search-string)
  (silver-brain--client-get (format "items?search=%s" search-string)))

(defun silver-brain-client-create-item (name content-type)
  (let* ((response (silver-brain--client-post "items"
                                  (list (cons "name" name)
                                        (cons "contentType" content-type))))
         (id (silver-brain--prop-id response))
         (item (silver-brain-client-get-item id)))
    item))

(cl-defun silver-brain-client-update-item (id &key name content-type content)
  (let ((data '()))
    (when name
      (push (cons "name" name) data))
    (when content-type
      (push (cons "contentType" content-type) data))
    (when content
      (push (cons "content" content) data))
    (silver-brain--client-patch (format "items/%s" id) data)))

(defun silver-brain-client-delete-item (id)
  (silver-brain--client-delete (format "items/%s" id)))

(defun silver-brain-client-create-child (id child-id)
  (silver-brain--client-post (format "items/%s/children/%s" id child-id)))

(defun silver-brain-client-delete-child (id child-id)
  (silver-brain--client-delete (format "items/%s/children/%s" id child-id)))

(defun silver-brain-client-create-reference (source target annotation)
  (let* ((response (silver-brain--client-post "references"
                                  (list (cons "source" source)
                                        (cons "target" target)
                                        (cons "annotation" annotation))))
         (reference-id (silver-brain--prop-id response)))
    reference-id))

(defun silver-brain-client-get-references (ids)
  (silver-brain--client-get (format "references?ids=%s" (string-join ids ","))))

(defun silver-brain-client-update-reference (id annotation)
  (silver-brain--client-patch (format "references/%s" id)
                  (list (cons "annotation" annotation))))

(defun silver-brain-client-delete-reference (id)
  (silver-brain--client-delete (format "references/%s" id)))

(provide 'silver-brain-client)
