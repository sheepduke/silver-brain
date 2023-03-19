(in-package #:silver-brain.global)

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

(with-auto-export ()
  (defun setup ()
    (setf shasht:*symbol-name-function*
          #'slot-name->json-key)
    (unlisp.dev:setup)))
