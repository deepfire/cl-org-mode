;;; -*- lisp -*-

(defsystem :cl-org-mode-extended
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:module :extended
                      :serial t
                      :components
                      ((:file "packages")
                       (:file "extended")
                       ))
	     )))
  :depends-on (:cl-org-mode :ironclad-text))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :cl-org-mode-extended))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for CL-ORG-MODE-EXTENDED-TESTS")))
