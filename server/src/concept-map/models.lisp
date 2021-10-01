(in-package silver-brain.concept-map)

(defstruct concept
  uuid name
  content-type content
  created-at updated-at
  links-in links-out links-by)
