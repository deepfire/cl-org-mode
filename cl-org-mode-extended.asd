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
                       (:file "extended")
                       ))
	     )))
  :depends-on (:cl-org-mode :ironclad-text))
