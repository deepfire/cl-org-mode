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
  :in-order-to ((asdf:test-op (asdf:load-op :cl-org-mode-tests)))
  :perform (asdf:test-op :after (op c)
             (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
                 (error "TEST-OP failed for CL-ORG-MODE-TESTS")))
  :serial t
  :depends-on (:alexandria :iterate :cl-org-mode-raw :parser-combinators-debug))
