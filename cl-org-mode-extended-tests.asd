;;; -*- lisp -*-

(defsystem :cl-org-mode-extended-tests
  :serial t
  :components
  ((:module :t
	    :serial t
	    :components
            ((:file "extended"))))
  :depends-on (:cl-org-mode-tests
               :cl-org-mode-extended))
