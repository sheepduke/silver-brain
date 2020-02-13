;;; Package --- silver-brain-api

;;; Commentary:
;;;
;;; This package communicate with API provided by the back-end server.

;;; Code:

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

(defun silver-brain-api--search-concept (keyword)
  "Search concept by KEYWORD.
Returns a list of concepts."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get (format "/concepts?search=%s" keyword))))

(defun silver-brain-api--update-concept (concept)
  "Save CONCEPT to the server."
  (silver-brain-api--put
   (concat "/concepts/" (silver-brain-concept-uuid concept))
   (json-encode-plist
    (silver-brain-api--concept-to-plist concept))))

(defun silver-brain-api--delete-concept (concept)
  "Delete CONCEPT from the server."
  (silver-brain-api--delete
   (concat "/concepts/" (silver-brain-concept-uuid concept))))

(defun silver-brain-api--get-relation (relation uuid)
  "Return a list of concepts that are RELATION of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get
           (concat "/concepts/" uuid "/"
                   (silver-brain-api--relation-to-url relation)))))

(defun silver-brain-api--add-relation (relation uuid target-uuid)
  "Add TARGET-UUID as a relation of UUID by given RELATION."
  (silver-brain-api--put
   (concat "/concepts/" uuid
           "/" (silver-brain-api--relation-to-url relation)
           "/" target-uuid)))

(defun silver-brain-api--remove-relation (relation uuid target-uuid)
  "Remove TARGET-UUID from relation of UUID by RELATION."
  (silver-brain-api--delete
   (concat "/concepts/" uuid
           "/" (silver-brain-api--relation-to-url relation)
           "/" target-uuid)))

(defun silver-brain-api--relation-to-url (relation)
  "Convert RELATION to corresponding URL."
  (case relation
                ('parent "parents")
                ('child "children")
                ('friend "friends")
                (t (error "Invalid relation: %s" relation))))

(defun silver-brain-api--get (url &optional params)
  "Send GET request to server with given URI.
Return the result as property list with following conversion rules:
* Key => keyword
* Array => list."
  (http-response-data (http-request (silver-brain-api--url url))))

(cl-defun silver-brain-api--request (url &key (method :get) data)
  "Send request to URL with given DATA."
  (http-request (silver-brain-api--url url) :method method :data data))

(defun silver-brain-api--post (url &optional data)
  "Send POST request to the server with given URL and DATA.
Returns the value of Location header."
  (http-response-location
   (http-request (silver-brain-api--url url)
                 :method :post
                 :data data)))

(defun silver-brain-api--put (url &optional data)
  "Send PUT request to the server with given URL and DATA."
  (http-request (silver-brain-api--url url)
                :method :put
                :data data))

(defun silver-brain-api--delete (url)
  "Send DELETE request to the server with given URL."
  (http-request (silver-brain-api--url url)
                :method :delete))

(cl-defun http-request (url &key (method :get) (data nil))
  "Send HTTP request to URL with given METHOD and DATA."
  (let ((url-request-method (case method
                              (:get "GET")
                              (:post "POST")
                              (:put "PUT")
                              (:delete "DELETE")))
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (buffer-string))))

(defun http-response-data (response)
  "Extract HTTP body from RESPONSE and decode it."
  (let* ((json-object-type 'plist)
         (json-key-type 'keyword)
         (json-array-type 'list))
    (with-temp-buffer
      (insert (http-response-body response))
      (goto-char (point-min))
      (json-read))))

(defun http-response-body (response)
  "Extract HTTP body from response."
  (cl-subseq response (+ 2 (cl-search "\n\n" response))))

(defun http-response-location (response)
  "Extract HTTP Location header from response."
  (let* ((start (cl-search "Location: " response))
         (middle (+ 2 (cl-search ": " response :start2 start)))
         (end (cl-search "\n" response :start2 start))
         (match (cl-subseq response start end)))
    (cl-subseq response middle end)))

(defun silver-brain-api--url (uri)
  "Convert given resource URI to full URL."
  (format "http://localhost:%s/api%s" silver-brain-server-port uri))

(defun silver-brain-api--plist-to-concept (plist)
  "Return a CONCEPT instance from PLIST."
  (make-silver-brain-concept :uuid (cl-getf plist :uuid)
                             :name (cl-getf plist :name)
                             :content (cl-getf plist :content)
                             :content-format (cl-getf plist :contentFormat)))

(defun silver-brain-api--concept-to-plist (concept)
  "Return a property list from CONCEPT."
  (list :uuid (silver-brain-concept-uuid concept)
        :name (silver-brain-concept-name concept)
        :content (silver-brain-concept-content concept)
        :content-format (silver-brain-concept-content-format concept)))

(provide 'silver-brain-api)

;;; silver-brain-api.el ends here
