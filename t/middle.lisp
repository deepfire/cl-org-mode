(defpackage :cl-org-mode-tests-middle
  (:use :cl
        :alexandria
        :iterate
	:cl-org-mode))

(in-package :cl-org-mode-tests-middle)

(rtest:deftest :middle/org-dress (length (mapcar #'org-parse cl-org-mode-tests-raw::*org-files*)) 3)
(rtest:deftest :middle/org-present (org-present :flat (org-parse (lastcar cl-org-mode-tests-raw::*org-files*)) (make-broadcast-stream)) nil)
