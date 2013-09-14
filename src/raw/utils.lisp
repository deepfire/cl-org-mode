(in-package :cl-org-mode-raw)

(define-constant +newline-string+ (coerce #(#\Newline) 'string) :test #'equal)

;; from pergamum
(defun unzip (fn sequence &key (key #'identity))
  (iter (for elt in sequence)
        (if (funcall fn (funcall key elt))
            (collect elt into yes)
            (collect elt into no))
        (finally (return (values yes no)))))

;; from pergamum
(defun strconcat (xs)
  (apply #'concatenate 'string xs))

;; from pergamum
(defun strconcat* (&rest xs)
  (strconcat xs))
