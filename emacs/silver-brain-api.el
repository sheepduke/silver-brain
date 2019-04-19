;;; Package --- silver-brain-api

;;; Commentary:
;;;
;;; This package communicate with API provided by the back-end server.

;;; Code:

(require 'url)

(defvar silver-brain-server-port 5000
  "The port of Silver Brain server.")

(defun silver-brain-api--get-all-concepts ()
  "Return all concepts."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get "/concepts")))

(defun silver-brain-api--get-concept (uuid)
  "Get specific concept by UUID."
  (silver-brain-api--plist-to-concept
   (silver-brain-api--get (concat "/concepts/" uuid))))

(defun silver-brain-api--create-concept (name content-format)
  "Create a concept with given NAME and CONTENT-FORMAT.
Returns the newly created concept."
  (silver-brain-api--plist-to-concept
   (silver-brain-api--get
    (silver-brain-api--post
     "/concepts"
     (json-encode-plist (list :name name
                              :content ""
                              :content-format content-format))))))

(defun silver-brain-api--update-concept (concept)
  "Save CONCEPT to the server."
  (silver-brain-api--put
   (concat "/concepts/" (silver-brain-concept-uuid concept))
   (json-encode-plist
    (silver-brain-api--concept-to-plist concept))))

(defun silver-brain-api--get-parents (uuid)
  "Return a list of parents of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get (concat "/concepts/" uuid "/parents"))))

(defun silver-brain-api--get-children (uuid)
  "Return a list of children of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
   (silver-brain-api--get (concat "/concepts/" uuid "/children"))))

(defun silver-brain-api--get-friends (uuid)
  "Return a list of friends of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get (concat "/concepts/" uuid "/friends"))))

(defun silver-brain-api--get (uri)
  "Send GET request to server with given URI.
Return the result as property list with following conversion rules:
* Key => keyword
* Array => list."
  (with-current-buffer (url-retrieve-synchronously (silver-brain-api--url uri))
    ;; (goto-char (point-min))
    (re-search-backward "^$")
    (let* ((json-object-type 'plist)
           (json-key-type 'keyword)
           (json-array-type 'list)
           (data (json-read)))
      data)))

(defun silver-brain-api--post (url &optional data)
  "Send POST request to the server with given URL and DATA.
Returns the value of Location header."
  (let ((url-request-method "POST")
        (url-request-data data))
    (with-current-buffer
        (url-retrieve-synchronously (silver-brain-api--url url))
      (beginning-of-buffer)
      (re-search-forward "Location: ")
      (buffer-substring (point) (line-end-position)))))

(defun silver-brain-api--put (url &optional data)
  "Send PUT request to the server with given URL and DATA."
  (let ((url-request-method "PUT")
        (url-request-data data)))
  (url-retrieve-synchronously (silver-brain-api--url url)))

(defun silver-brain-api--url (uri)
  "Convert given resource URI to full URL."
  (format "http://localhost:%s%s" silver-brain-server-port uri))

(defun silver-brain-api--plist-to-concept (plist)
  "Return a CONCEPT instance from PLIST."
  (make-silver-brain-concept :uuid (getf plist :uuid)
                             :name (getf plist :name)
                             :content (getf plist :content)
                             :content-format (getf plist :contentFormat)))

(defun silver-brain-api--concept-to-plist (concept)
  "Return a property list from CONCEPT."
  (list :uuid (silver-brain-concept-uuid concept)
        :name (silver-brain-concept-name concept)
        :content (silver-brain-concept-content concept)
        :content-format (silver-brain-concept-content-format concept)))

(provide 'silver-brain-api)

;;; silver-brain-api.el ends here
