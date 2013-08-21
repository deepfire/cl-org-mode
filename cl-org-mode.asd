;;; -*- lisp -*-

(defsystem :cl-org-mode
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "utils")
	     (:file "protocol")
	     (:file "parser")
	     (:file "new-parser")
	     (:file "cl-org-mode")
	     )))
  :serial t
  :depends-on (:alexandria :closer-mop :parser-combinators :split-sequence))
