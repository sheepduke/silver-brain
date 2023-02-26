(unlisp.prelude:defpackage #:silver-brain-tests.shared.data.v1
  (:use #:unlisp.prelude)
  (:local-nicknames (#:v1 #:silver-brain.store.schema.v1)))

(in-package #:silver-brain-tests.shared.data.v1)

(with-auto-export ()
  (def mock-concept-01 (make-instance 'v1:concept
                                      :uuid "0x01"
                                      :name "First one"
                                      :content-format "text/org"
                                      :content "Org content"))

  (def mock-concept-02 (make-instance 'v1:concept
                                      :uuid "0x02"
                                      :name "Second one"
                                      :content-format "text/md"
                                      :content "Markdown content"))

  (def mock-concept-03 (make-instance 'v1:concept
                                      :uuid "0x03"
                                      :name "Third one"))

  (def mock-concepts (list mock-concept-01 mock-concept-02 mock-concept-03))

  (def mock-relation-01->02 (make-instance 'v1:concept-relation
                                           :source "0x01"
                                           :target "0x02"))

  (def mock-relation-01->03 (make-instance 'v1:concept-relation
                                           :source "0x01"
                                           :target "0x03"))

  (def mock-relation-03->01 (make-instance 'v1:concept-relation
                                           :source "0x03"
                                           :target "0x01"))

  (def mock-concept-relations (list mock-relation-01->02
                                    mock-relation-01->03
                                    mock-relation-03->01)))
