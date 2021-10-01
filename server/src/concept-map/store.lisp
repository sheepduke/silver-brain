(defpackage silver-brain.concept-map.store
  (:use #:cl)
  (:local-nicknames (#:cache #:silver-brain.concept-map.cache)))

(in-package silver-brain.concept-map.store)

(defun get-concept-by-uuid (uuid)
  (let ((concept (mito:find-dao 'store:concept :uuid uuid)))
    (make-concept :uuid (store:concept-uuid concept)
                  :name (store:concept-name concept)
                  :content-type (store:concept-content-type concept)
                  :content (store:concept-content concept)
                  :created-at (store:object-created-at concept)
                  :updated-at (store:object-updated-at concept))))

(defun get-concept-summary-by-uuid (uuid)
  )

(defun get-links-in (uuid)
  (let ((links (mito:select-dao 'store:concept-link
                 (sxql:where (:= :target uuid)))))))

(defun get-links-out (uuid)
  (mito:select-dao 'store:concept-link
    (sxql:where (:= :source uuid))))


;; (get-links-out "5BAAB06F-D70D-4405-8511-3032D12448B3")


(defstruct cached-concept uuid name links-in links-out links-at)


(defun load-cached-concept (uuid)
  (let ((concept (mito:select-dao 'store:concept (sxql:where (:= :uuid uuid))))
        (links-in (mito:select-dao 'store:concept (sxql:where (:= :target uuid))))
        (links-out (mito:select-dao 'store:concept (sxql:where (:= :source uuid))))
        (links-at (mito:select-dao 'store:concept (sxql:where (:= :uuid uuid)))))
    ))

;; ----------------------------------------------------------------------

;; (defvar *uuid* "5BAAB06F-D70D-4405-8511-3032D12448B3")
;; (load-cached-concept *uuid*)
