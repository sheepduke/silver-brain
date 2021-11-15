;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*- 

(defun silver-brain-dev-unload ()
  (interactive)
  (ignore-errors 
    (unload-feature 'silver-brain))
  (ignore-errors
    (unload-feature 'silver-brain-hello))
  (ignore-errors
    (unload-feature 'silver-brain-list))
  (ignore-errors
    (unload-feature 'silver-brain-concept))
  (ignore-errors
    (unload-feature 'silver-brain-common))
  (ignore-errors
    (unload-feature 'silver-brain-vars)))

(defun silver-brain-dev-reload ()
  (interactive)
  (silver-brain-dev-unload)
  (require 'silver-brain)
  (require 'silver-brain-hello)
  (require 'silver-brain-list)
  (require 'silver-brain-concept)
  (require 'silver-brain-common)
  (require 'silver-brain-vars)
  (setq silver-brain-server-port 5001)
  (setq silver-brain-database-name "a"))
