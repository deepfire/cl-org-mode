(in-package :cl-org-mode-extended)

;;;
;;; Conditions
(define-condition org-dress-error (org-error)
  ())

(define-condition org-object-error (org-error)
  ((hash :reader hash-of :initarg :hash)))

(define-condition org-object-warning (org-warning)
  ((hash :reader hash-of :initarg :hash)))

(cl-org-mode::define-simple-error   org-object-error)
(cl-org-mode::define-simple-warning org-object-warning)

(define-condition org-object-relink-error (org-dress-error)
  ())
