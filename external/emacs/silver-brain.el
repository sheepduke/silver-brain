;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'silver-brain-list)
(require 'silver-brain-util)
(require 'silver-brain-item)

(require 'pretty-hydra)

;; ============================================================
;;  Hydra
;; ============================================================

(pretty-hydra-define silver-brain-hydra ()
  ("Item"
   (("c" silver-brain-create-and-open-item "create")
    ("o" silver-brain-search-and-open-item "open")
    ("l" silver-brain-list-items "list"))))

(provide 'silver-brain)

;;; silver-brain.el ends here
