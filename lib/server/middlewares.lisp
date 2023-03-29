(in-package #:silver-brain.server)

(def middleware-with-database
  (fun (app)
    (fun (env)
      (format t "Enter with-database~&")
      (let ((database-name (htbl:elt (plist:elt env :headers)
                                     "X-DatabaseName" "silver-brain")))
        (store:with-database database-name
          (funcall app env)))))
  "Middleware for establishing database connection from HTTP header X-DatabaseName.")

(defun slot-name->json-key (slot-name)
  (let ((capitalize? nil))
    (io:with-output-to-string (stream)
      (loop for char across (string:downcase (if (string:ends-with? slot-name "?")
                                                 (format nil "is-~A" slot-name)
                                                 slot-name))
            if (char:= char #\-)
              do (setf capitalize? t)
            else
              do (io:write-char (if capitalize?
                                    (char:upcase char)
                                    char)
                                stream)
                 (setf capitalize? nil)))))

(defmethod shasht:print-json-value ((value time:timestamp) stream)
  (format stream "\"~A\"" (time:to-rfc3339-timestring value)))

(def middleware-with-json-response
  (fun (app)
    (fun (env)
      (let ((shasht:*symbol-name-function* #'slot-name->json-key))
        (ematch (funcall app env)
          ((list status headers (list object))
           (list status headers (list (io:with-output-to-string (stream)
                                        (shasht:write-json object stream)))))
          (result result))))))

(def custom-middlewares
  (list middleware-with-database
        middleware-with-json-response))
