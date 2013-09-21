(in-package :cl-org-mode)


(define-condition org-condition ()                      ())
(define-condition org-error     (org-condition error)   ())
(define-condition org-warning   (org-condition warning) ())

(define-simple-error   org-error)
(define-simple-warning org-warning)

(define-condition org-parse-error (org-error)
  ((source   :reader source-of   :initarg :source)
   (line     :reader line-of     :initarg :line)
   (column   :reader column-of   :initarg :column)))
