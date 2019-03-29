(defpackage silver-brain.server
  (:use #:cl
        #:alexandria
        #:iterate)
  (:import-from #:cl-json
                #:encode-json-to-string)
  (:export #:start
           #:stop))
(in-package silver-brain.server)

(defvar *app* (make-instance 'ningle:app))
(defvar *server* nil)

(defvar *concept-map* (make-instance 'concept-map:concept-map))

(defmacro defroute (app url method &rest body)
  (with-gensyms (params-name)
    `(setf (ningle:route ,app ,url :method ,method)
           (lambda (,params-name)
             (print ,params-name)
             (labels ((get-param (key)
                      (assoc-value ,params-name key)))
               ,@body)))))


(defun extract-params (url)
  (multiple-value-bind (start end) (ppcre:scan ":[a-zA-Z_-]+" url)
    (if start
        (append (list (symbolicate (string-upcase (subseq url (1+ start) end))))
                (extract-params (subseq url end))))))

(defmacro defroute (app url method &body body)
  (with-gensyms (params-sym param-list-sym param-sym)
    `(setf (ningle:route ,app ,url :method ,method)
           (lambda (,params-sym)
             ,(let ((param-list-sym (extract-params url)))
                `(let ,(mapcar (lambda (param-sym)
                                 `(,(symbolicate param-sym)
                                   (assoc-value ,params-sym ,(alexandria:make-keyword param-sym))))
                        param-list-sym)
                   ,@body))))))

(defroute *app* "/concepts/:id/children/:child-id" :get
  (format nil "ID: ~a Child: ~a" id child-id))

(defroute *app* "/" :get
  "Hello")

(defroute *app* "/concepts" :get
  (encode-json-to-string 
   (iter (for (key value) in-hashtable (concept-map:concepts *concept-map*))
     (collect `((:uuid . ,(concept:uuid value))
                (:name . ,(concept:name value)))))))

(defroute *app* "/concepts/:id" :get
  "ID")

;; (concept-map:add-concept *concept-map*
;;                          (make-instance 'concept:concept :name "Software"))
;; (concept-map:add-concept *concept-map*
;;                          (make-instance 'concept:concept :name "Emacs"))

(defun start ()
  "Start the server."
  (setf *server* (clack:clackup *app*)))

(defun stop ()
  "Stop the server."
  (clack:stop *server*)
  (setf *server* nil))

;; (start)
