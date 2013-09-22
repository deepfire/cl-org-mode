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
                       (:file "conditions")
                       (:file "utils")
                       (:file "hash")
                       (:file "extended")
                       ))
	     )))
  :depends-on (:cl-org-mode :ironclad :flexi-streams))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :cl-org-mode-extended))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for CL-ORG-MODE-EXTENDED-TESTS")))
