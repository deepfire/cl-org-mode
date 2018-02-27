(in-package :cl-org-mode-raw)

(define-constant +newline-string+ (coerce #(#\Newline) 'string) :test #'equal)

;; from pergamum
(defun unzip (fn sequence &key (key #'identity))
  (iter (for elt in sequence)
        (if (funcall fn (funcall key elt))
            (collect elt into yes)
            (collect elt into no))
        (finally (return (values yes no)))))

(defmacro dotree ((name tree &optional ret-val) &body body)
  "Evaluate BODY with NAME bound to every element in TREE. Return RET-VAL."
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
                  (dolist (,list-element ,list)
                    (if (consp ,list-element)
                        (,traverser ,list-element)
                        (let ((,name ,list-element))
                          ,@body)))))
         (,traverser ,tree)
         ,ret-val))))

;; from pergamum
(defun strconcat (xs)
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (stream)
      (dotree (str xs)
        (when str
          (princ str stream))))))

;; from pergamum
(defun strconcat* (&rest xs)
  (strconcat xs))
