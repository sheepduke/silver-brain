;; Local Variables:
;; lexical-binding: t
;; nameless-current-name: "silver-brain-api"
;; End:

(require 'cl-lib)
(require 'json)
(require 'silver-brain-common)

(defvar silver-brain-database-name "a.sqlite")

(cl-defun silver-brain-api-send-request (uri &key (method :get) data)
  "Send HTTP request to URI with Database header set."
  (let ((url-request-extra-headers `(("Database" . ,silver-brain-database-name)))
        (url-request-method (cl-case method
                              (:get "GET")
                              (:post "POST")
                              (:patch "PATCH")
                              (:delete "DELETE")))
        (url-request-data data))
    (let ((buffer (url-retrieve-synchronously (format "http://localhost:%d/api/%s"
                                                      silver-brain-server-port
                                                      uri))))
      (with-current-buffer buffer
        (let ((code (silver-brain-api-status-code)))
          (unless (<= 200 code 299)
            (error (silver-brain-api-body-string)))))
      buffer)))

(defun silver-brain-api-status-code ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "HTTP/")
    (let ((end (search-forward-regexp "[0-9]\\{3\\}")))
      (car (read-from-string (buffer-substring (- end 3) end))))))

(defun silver-brain-api-body-string ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring (point) (point-max))))

(cl-defun silver-brain-api-read-json (&key (object-type 'alist)
                            (key-type 'keyword))
  "Read response body as JSON and parse it.
OBJECT-TYPE and KEY-TYPE is set to JSON-KEY-TYPE and JSON-ARRAY-TYPE."
  (let ((json-object-type object-type)
        (json-key-type key-type)
        (json-array-type 'list))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n\n")
      (json-read))))

(provide 'silver-brain-api)

;;; silver-brain-api.el ends here
