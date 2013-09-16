(defpackage :cl-org-mode-extended-tests
  (:use :cl
        :alexandria
        :iterate
        :cl-org-mode
	:cl-org-mode-extended))

(in-package :cl-org-mode-extended-tests)

(defun roundtrip (org)
  (with-hash-cache ()
    (org-parse-extended
     (with-output-to-string (s)
       (with-hash-cache (:force t)
         (org-present :normal (org-parse-extended org) s)))))
  t)

(rtest:deftest :extended/org-parse-extended (roundtrip (lastcar cl-org-mode-raw-tests::*org-files*)) t)
