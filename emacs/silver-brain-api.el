;;; Package --- silver-brain-api

;;; Commentary:
;;;
;;; This package communicate with API provided by the back-end server.

;;; Code:

(require 'request)

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
          (silver-brain-api--get "/concepts" `(("search" . ,keyword)))))

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

(defun silver-brain-api--get-parents (uuid)
  "Return a list of parents of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get (concat "/concepts/" uuid "/parents"))))

(defun silver-brain-api--add-parent (uuid parent-uuid)
  "Add PARENT-UUID as a parent of UUID."
  (silver-brain-api--put (concat "/concepts/" uuid "/parents/" parent-uuid)))

(defun silver-brain-api--get-children (uuid)
  "Return a list of children of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
   (silver-brain-api--get (concat "/concepts/" uuid "/children"))))

(defun silver-brain-api--get-friends (uuid)
  "Return a list of friends of concept UUID."
  (mapcar #'silver-brain-api--plist-to-concept
          (silver-brain-api--get (concat "/concepts/" uuid "/friends"))))

(defun silver-brain-api--get (url &optional params)
  "Send GET request to server with given URI.
Return the result as property list with following conversion rules:
* Key => keyword
* Array => list."
  (request-response-data
   (request (silver-brain-api--url url)
            :parser (lambda ()
                      (let* ((json-object-type 'plist)
                             (json-key-type 'keyword)
                             (json-array-type 'list))
                        (json-read)))
            :params params
            :sync t)))

(defun silver-brain-api--post (url &optional data)
  "Send POST request to the server with given URL and DATA.
Returns the value of Location header."
  (request-response-header (request (silver-brain-api--url url)
                                    :type "POST"
                                    :data data
                                    :sync t)
                           "Location"))

(defun silver-brain-api--put (url &optional data)
  "Send PUT request to the server with given URL and DATA."
  (request (silver-brain-api--url url)
           :type "PUT"
           :data data
           :sync t))

(defun silver-brain-api--delete (url)
  "Send DELETE request to the server with given URL."
  (request (silver-brain-api--url url)
           :type "DELETE"
           :sync t))

(defun silver-brain-api--url (uri)
  "Convert given resource URI to full URL."
  (format "http://localhost:%s%s" silver-brain-server-port uri))

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
