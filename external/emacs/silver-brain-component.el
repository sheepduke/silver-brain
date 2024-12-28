;;; -*- lexical-binding: t; nameless-current-name: "silver-brain" -*-

(require 'seq)

(require 'silver-brain-prop)
(require 'silver-brain-client)

(defun silver-brain-insert-item-button (item action)
  "Insert a text button with ITEM name as its label. The ACTION is a
function that takes item id as its argument and perform corresponding
operation."
  (insert-text-button (silver-brain--prop-name item)
                      'action (lambda (_)
                                (funcall action (silver-brain--prop-id item)))
                      'item-id (silver-brain--prop-id item)))

(defun silver-brain-search-and-select-item (search-string)
  "Search with SEARCH-STRING and return a list of ."
  (silver-brain-select-item (silver-brain-client-search-items search-string)))

(defun silver-brain-select-item (items)
  "Select one item from given ITEMS."
  (let ((item-map (seq-map (lambda (item)
                             (cons (format "%s [%s]"
                                           (silver-brain--prop-name item)
                                           (silver-brain--prop-id item))
                                   (silver-brain--prop-id item)))
                           (silver-brain-sort-items items))))
    (or (and items
             (cdr (assoc-string (completing-read "Choose item: " item-map)
                                item-map)))
        (error "No item found"))))

(defun silver-brain-sort-items (items)
  "Sort ITEMS by id and then name."
  (seq-sort-by #'silver-brain--prop-name #'string< 
               (seq-sort-by #'silver-brain--prop-id #'string< items)))

(provide 'silver-brain-component)
