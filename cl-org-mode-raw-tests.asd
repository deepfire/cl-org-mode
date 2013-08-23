;;; -*- lisp -*-

(defsystem :cl-org-mode-raw-tests
  :components 
  ((:module :src
	    :serial t
	    :components
	    ((:module :raw
                      :serial t
                      :components
                      ((:file "tests")
                       )))))
  :serial t
  :depends-on (:cl-org-mode-raw :parser-combinators-debug))
