;;; -*- lisp -*-

(defsystem :cl-org-mode-tests
  :serial t
  :components
  ((:module :t
	    :serial t
	    :components
            ((:file "raw")
             (:file "middle"))))
  :depends-on (:cl-org-mode-raw :parser-combinators-debug
               :cl-org-mode :rt))
