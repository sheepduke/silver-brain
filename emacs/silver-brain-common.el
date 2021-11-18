;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)

(require 'silver-brain-vars)

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'silver-brain-new-concept)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "Q") 'silver-brain-quit-all)
    (define-key map [remap self-insert-command] 'silver-brain--no-edit)
    map)
  "Common keymap shared by all the Silver Brain buffers.")

(defun silver-brain--no-edit ()
  "Refuse to allow editing of Custom buffer."
  (interactive)
  (error "Undefined key binding"))

(defun silver-brain-quit-all ()
  "Kill all the Silver Brain buffers."
  (interactive)
  (mapc (lambda (buffer) (kill-buffer buffer))
        (cl-remove-if-not (lambda (buffer)
                            (string-prefix-p "*Silver Brain" (buffer-name buffer)))
                          (buffer-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                            Basic                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro silver-brain--with-widget-buffer (buffer-name &rest body)
  "Wrap basic buffer setup functions."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create ,buffer-name)
     (let ((inhibit-read-only t))
       (mapc 'widget-delete widget-field-list)
       (erase-buffer)
       ,@body
       (widget-setup)
       (set-buffer-modified-p nil)
       (goto-char (point-min)))))

(defun silver-brain--get-textfield-length (length)
  "Return the width of text field widget. LENGTH is the extra
length to be removed."
  (max 8 (- (min 80 (window-width)) 10 length)))

(defun silver-brain--time-to-string (time)
  "Convert given TIME to string. TIME is a timestamp."
  (format-time-string silver-brain-time-format time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Buffer Style                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro silver-brain--with-concept-hyperlink-face (&rest body)
  `(let ((widget-button-face 'silver-brain-concept-hyperlink)
         (widget-push-button-prefix nil)
         (widget-push-button-suffix nil))
     ,@body))

(defmacro silver-brain--with-push-button-face (&rest body)
  `(let ((widget-button-face 'silver-brain-push-button)
         (widget-push-button-prefix " ")
         (widget-push-button-suffix " "))
     ,@body))

(defun silver-brain--widget-insert-with-face (text face)
  (let ((start (point)))
    (widget-insert text)
    (let ((end (point)))
      (add-face-text-property start end face))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                           Request                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun silver-brain--api-send-request (uri &key (method :get) data)
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
        (let ((code (silver-brain--api-status-code)))
          (unless (<= 200 code 299)
            (error (format "Server response %d: %s"
                           code
                           (string-trim (silver-brain--api-body-string)))))))
      buffer)))

(defun silver-brain--api-status-code ()
  "Extract the HTTP status code in number format from response."
  (save-excursion
    (goto-char (point-min))
    (search-forward "HTTP/")
    (let ((end (search-forward-regexp "[0-9]\\{3\\}")))
      (car (read-from-string (buffer-substring (- end 3) end))))))

(defun silver-brain--api-body-string ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (buffer-substring (point) (point-max))))

(cl-defun silver-brain--api-read-json (&key (object-type 'alist)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          Data Model                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct silver-brain-concept uuid name content-type content created-at updated-at)

(defun silver-brain-concept-from-alist (alist)
  "Create concept from alist."
  (let* ((keys '(:uuid :name :content-type :content))
         (initargs (cl-reduce (lambda (acc key)
                                (append acc (list key (alist-get key alist))))
                              keys
                              :initial-value '()))
         (concept (apply #'make-silver-brain-concept initargs)))
    (if-let (create-time (alist-get :created-at alist))
        (setf (silver-brain-concept-created-at concept)
              (encode-time (iso8601-parse create-time))))
    (if-let (update-time (alist-get :updated-at alist))
        (setf (silver-brain-concept-updated-at concept)
              (encode-time (iso8601-parse update-time))))
    concept))

(cl-defstruct silver-brain-concept-summary uuid name)

(defun silver-brain-concept-summary-from-alist (alist)
  "Create concept-summary from alist."
  (make-silver-brain-concept-summary :uuid (alist-get :uuid alist)
                                     :name (alist-get :name alist)))

(cl-defstruct silver-brain-concept-link source relation target)

(defun silver-brain-concept-link-from-alist (alist)
  "Create concept-link object from given ALIST."
  (make-silver-brain-concept-link
   :source (silver-brain-concept-summary-from-alist (alist-get :source alist))
   :relation (silver-brain-concept-summary-from-alist (alist-get :relation alist))
   :target (silver-brain-concept-summary-from-alist (alist-get :target alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Api                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun silver-brain-switch-database ()
  (interactive)
  (let* ((database-list (silver-brain-get-database-list))
         (database (completing-read "Switch to: " database-list)))
    (silver-brain-quit-all)
    (setq silver-brain-database-name database)))

(defun silver-brain-get-database-list ()
  "Return a list of databases."
  (with-current-buffer (silver-brain--api-send-request "database")
    (silver-brain--api-read-json)))

(cl-defun silver-brain-new-concept (&optional name)
  "Create a new concept. If NAME is given, it is used as the name
of new concept. Otherwise, it prompts the user to input one."
  (interactive)
  (let* ((name (or name (read-string "Concept name: ")))
         uuid)
    (with-current-buffer (silver-brain--api-send-request
                          "concept"
                          :method :post
                          :data (json-encode
                                 `((:name . ,name)
                                   (:content-type . ,silver-brain-default-content-type))))
      (setq uuid (silver-brain--api-body-string)))
    (run-hooks 'silver-brain-after-concept-create-hook)
    (silver-brain-concept-show uuid)))

(cl-defun silver-brain-delete-concept (uuid)
  "Delete concept. If UUID is given, it is used to specify the
target concept. Otherwise, silver-brain-current-concept will be
deleted."
  (let ((uuid (or uuid
                  (and (or silver-brain-current-concept
                           (error "Not invoked in Silver Brain Concept buffer"))
                       (silver-brain-concept-uuid silver-brain-current-concept)))))
    (and uuid
         (with-current-buffer (silver-brain--api-send-request
                               (concat "concept/" uuid)
                               :method :delete))
         (run-hooks 'silver-brain-after-concept-delete-hook)
         t)))

(defun silver-brain-delete-link (source relation target)
  "Delete link with RELATION between SOURCE and TARGET."
  (silver-brain--api-send-request (format "concept-link?source=%s&relation=%s&target=%s"
                              source relation target)
                      :method :delete)
  (run-hooks 'silver-brain-after-concept-update-hook))

(defun silver-brain--search-concept (search-string)
  "Search concept with SEARCH-STRING and return a list of
concept-summary in sorted order."
  (with-current-buffer (silver-brain--api-send-request
                        (format "concept?search=%s" search-string))
    (thread-first (mapcar #'silver-brain-concept-summary-from-alist (silver-brain--api-read-json))
      (sort (lambda (s1 s2)
              (string< (silver-brain-concept-summary-uuid s1)
                       (silver-brain-concept-summary-uuid s2))))
      (sort (lambda (s1 s2)
              (string< (silver-brain-concept-summary-name s1)
                       (silver-brain-concept-summary-name s2)))))))


(cl-defun silver-brain--search-concept-and-select (&optional (prompt "Search string: "))
  "Ask for a search string, search for concepts and select
one. PROMPT is the prompt for search string."
  (let* ((result (silver-brain--search-concept (read-string prompt)))
         (concepts (mapcar (lambda (alist) (cons (silver-brain-concept-summary-name alist)
                                                 (silver-brain-concept-summary-uuid alist)))
                           result)))
    (and concepts
         (let ((key (completing-read "Choose concept: " concepts)))
           (cdr (assoc-string key concepts))))))

(defun silver-brain--new-link (source relation target)
  "Create a new link from SOURCE to TARGET with RELATION as the edge."
  (silver-brain--api-send-request "concept-link"
                      :method :post
                      :data (json-encode-list
                             `((("source" . ,source)
                                ("relation" . ,relation)
                                ("target" . ,target)))))
  (run-hooks 'silver-brain-after-concept-update-hook))

(provide 'silver-brain-common)
