(defpackage silver-brain/tests/concept
  (:use #:cl
        #:silver-brain.concept
        #:rove))
(in-package :silver-brain/tests/concept)

(deftest test-concept 
  (let ((software (make-instance 'concept :name "Software"))
        (editor (make-instance 'concept :name "Editor"))
        (emacs (make-instance 'concept :name "Emacs"))
        (vim (make-instance 'concept :name "Vim")))
    (testing "equals"
      (ok (string= (id software) (id software)))
      (ok (string-not-equal (id software) (id emacs))))
    
    (testing "become-child"
      (become-child software editor)
      (become-child editor emacs)
      (ok (childp editor software))
      (ok (parentp editor emacs)))

    (testing "remove-child"
      (become-child emacs vim)
      (ok (parentp emacs vim))
      (remove-child vim emacs)
      (ok (not (childp vim emacs))))

    (testing "become-friend"
      (become-friend emacs vim)
      (ok (not (childp emacs vim)))
      (ok (not (parentp emacs vim)))
      (ok (not (parentp emacs vim)))
      (ok (not (parentp vim emacs)))
      (ok (friendp emacs vim)))))
