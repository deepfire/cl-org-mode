;;; -*- lisp -*-

(defsystem :cl-org-mode
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "protocol")
             (:file "utils")
	     (:file "conditions")
	     (:file "cl-org-mode")
	     (:file "dress")
	     (:file "traversal")
	     (:file "present")
	     )))
  :serial t
  :depends-on (:alexandria :iterate :cl-org-mode-raw :parser-combinators-debug))

(defmethod asdf:perform ((op asdf:test-op)
                         (c (eql (asdf:find-system :cl-org-mode))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for CL-ORG-MODE-TESTS")))
