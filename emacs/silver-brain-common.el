;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'cl-lib)
(require 'json)

(require 'silver-brain-vars)

(defvar silver-brain-common-keymap
  (let ((map (make-sparse-keymap)))
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
        (remove-if-not (lambda (buffer)
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
  (max 8 (- (window-width) 10 length)))

(defun silver-brain--alist-set (key alist value)
  "Set VALUE of KEY in ALIST."
  (setf (cdr (assoc key alist)) value))

(defun silver-brain--time-to-string (time-string)
  "Display "
  (format-time-string silver-brain-time-format
                      (encode-time
                       (iso8601-parse time-string))))

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
;;;;                             Api                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(cl-defun silver-brain-delete-concept (&key uuid)
  "Delete concept. If UUID is given, it is used to specify the
target concept. Otherwise, silver-brain-current-concept will be
deleted."
  (interactive)
  (let ((uuid (or uuid
                  (and (or silver-brain-current-concept
                           (error "Not invoked in Silver Brain Concept buffer"))
                       (alist-get :uuid silver-brain-current-concept)))))
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
  (with-current-buffer (silver-brain--api-send-request
                        (format "concept?search=%s" search-string))
    (silver-brain--api-read-json)))

(cl-defun silver-brain--search-concept-and-select (&optional (prompt "Search string: "))
  "Ask for a search string, search for concepts and select
one. PROMPT is the prompt for search string."
  (let* ((result (silver-brain--search-concept (read-string prompt)))
         (concepts (mapcar (lambda (alist) (cons (alist-get :name alist)
                                                 (alist-get :uuid alist)))
                           result)))
    (and concepts
         (let ((key (completing-read "Choose concept: " concepts)))
           (cdr (assoc-string key concepts))))))

(defun silver-brain--new-link (source relation target)
  (silver-brain--api-send-request "concept-link"
                      :method :post
                      :data (json-encode-list
                             `((("source" . ,source)
                                ("relation" . ,relation)
                                ("target" . ,target)))))
  (run-hooks 'silver-brain-after-concept-update-hook))

(provide 'silver-brain-common)
