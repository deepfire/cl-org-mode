;;; -*- lisp -*-

(defsystem :cl-org-mode-tests
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:file "new-parser-tests")
	     )))
  :serial t
  :depends-on (:cl-org-mode :parser-combinators-debug))
