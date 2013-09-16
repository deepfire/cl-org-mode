(defpackage :cl-org-mode-middle-tests
  (:use :cl
        :alexandria
        :iterate
	:cl-org-mode))

(in-package :cl-org-mode-middle-tests)

(rtest:deftest :middle/org-dress (length (mapcar #'org-parse cl-org-mode-raw-tests::*org-files*)) 3)
(rtest:deftest :middle/org-present (org-present :flat (org-parse (lastcar cl-org-mode-raw-tests::*org-files*)) (make-broadcast-stream)) nil)
