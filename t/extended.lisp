(defpackage :cl-org-mode-extended-tests
  (:use :cl
        :alexandria
        :iterate
        :cl-org-mode
        :cl-org-mode-utils
	:cl-org-mode-extended))

(in-package :cl-org-mode-extended-tests)

(defun roundtrip-hash-stability (org)
  (let* ((initial (read-file-into-string (asdf:system-relative-pathname (asdf:find-system :cl-org-mode) #P"t/example.org")))
         (first (with-ignored-warnings (org-object-warning)
                  (org-parse-extended initial)))
         (first-rt (with-output-to-string (s)
                     (org-present :normal first s)))
         (second (org-parse-extended first-rt))
         (second-rt (with-output-to-string (s)
                      (org-present :normal second s))))
    (= (with-hash-cache ()
         (hash-of first))
       (with-hash-cache ()
         (hash-of second)))))

(defun roundtrip-text-stability (org)
  (let* ((initial (read-file-into-string (asdf:system-relative-pathname (asdf:find-system :cl-org-mode) #P"t/example.org")))
         (first (with-ignored-warnings (org-object-warning)
                  (org-parse-extended initial)))
         (first-rt (with-output-to-string (s)
                     (org-present :normal first s)))
         (second (org-parse-extended first-rt))
         (second-rt (with-output-to-string (s)
                      (org-present :normal second s))))
    (string= first-rt second-rt)))

(rtest:deftest :extended/roundtrip-hash (roundtrip-hash-stability (lastcar cl-org-mode-raw-tests::*org-files*)) t)
(rtest:deftest :extended/roundtrip-text (roundtrip-text-stability (lastcar cl-org-mode-raw-tests::*org-files*)) t)
