;;; -*- lisp -*-

(defsystem :cl-org-mode
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "utils")
	     (:file "protocol")
	     (:file "cl-org-mode")
	     )))
  :serial t
  :depends-on (:alexandria :iterate :cl-org-mode-raw :parser-combinators-debug))
