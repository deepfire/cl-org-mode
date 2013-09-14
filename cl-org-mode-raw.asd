;;; -*- lisp -*-

(defsystem :cl-org-mode-raw
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:module :raw
                      :serial t
                      :components
                      ((:file "packages")
                       (:file "utils")
                       (:file "parser")
                       ))
	     )))
  :serial t
  :depends-on (:alexandria :parser-combinators :split-sequence))
