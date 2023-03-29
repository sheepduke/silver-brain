(in-package #:silver-brain.server)

(defun true? (params key)
  (let ((value (jingle:get-request-param params key nil)))
    (and value
         (string:= value "true" :ignore-case? t))))

(defun get-concept (params)
  "Get concept by its UUID."
  (let ((uuid (jingle:get-request-param params :uuid))
        (load-aliases? (true? params "load-aliases"))
        (load-attachments? (true? params "load-attachments"))
        (load-times? (true? params "load-times")))
    (concept-map:get-concept uuid
                             :load-aliases? load-aliases?
                             :load-attachments? load-attachments?
                             :load-times? load-times?)))

(defun register-routes (app)
  ;; Redirects.
  (jingle:redirect-route app "/" "/swagger/")

  ;; Swagger.
  (jingle:serve-directory app "/swagger"
                          (path:join (global:store/root-path) "swagger/"))

  ;; Concept.
  (setf (jingle:route app "/api/v2/concepts/:uuid") #'get-concept))
