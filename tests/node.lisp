(defpackage silver-brain/tests/node
  (:use #:cl
        #:silver-brain.node
        #:rove))
(in-package :silver-brain/tests/node)

(deftest test-node 
  (let ((software (node:new "Software"))
        (editor (node:new "Editor"))
        (emacs (node:new "Emacs"))
        (vim (node:new "Vim")))
    (testing "equals"
      (ok (string= (uuid software) (uuid software)))
      (ok (string-not-equal (uuid software) (uuid emacs))))
    
    (testing "become-child"
      (become-child editor software)
      (become-child emacs editor)
      (ok (childp editor software))
      (ok (parentp editor emacs)))

    (testing "remove-child"
      (become-child vim emacs)
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
