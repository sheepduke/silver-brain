(in-package #:silver-brain.server)

(def dev-settings
  (make-instance 'global:settings
                 :store/root-path (path:join (path:user-home)
                                             ".silver-brain.dev/")))

(def middlewares
  (list:concat (list lack.middleware.accesslog:*lack-middleware-accesslog*
                     lack.middleware.backtrace:*lack-middleware-backtrace*)
               custom-middlewares))

(def web-dev-app
  (jingle:make-app :address "127.0.0.1"
                   :port 5050
                   :middlewares middlewares
                   :debug-mode t
                   :silent-mode nil
                   :use-thread t))

(defun start-dev-server ()
  (asdf:load-system 'silver-brain-tests.common)

  (let ((database-name "silver-brain"))
    (setf global:*settings* dev-settings)
    (store:ensure-data-directories-exist)
    (os:ensure-file-deleted (global:store/database-path database-name))
    (store:with-database database-name
      (store:migrate)
      (pack:symbol-call 'silver-brain-tests.common.data.v2 'context (op))
      (register-routes web-dev-app)
      (jingle:start web-dev-app))))

(defun stop-dev-server ()
  (when (jingle:http-server web-dev-app)
    (jingle:stop web-dev-app)))

;; (register-routes web-dev-app)
;; (dex:get "http://localhost:5050")
;; (dex:get "http://localhost:5050/api/v2/concepts/0011?load-aliases=true&load-times=true")
;; (dex:get "http://localhost:5050/api/v2/concept-links/0011")
;; (start-dev-server)
;; (stop-dev-server)
;; (dex:get "http://localhost:5050/")
;; (asdf:load-system "silver-brain.server" :force t)
;; (asdf:load-system 'dexador)
;; (asdf:test-system 'silver-brain.store)
;; (asdf:test-system 'silver-brain.concept-map)
