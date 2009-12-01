(in-package :cl-org-mode)

(defclass lisp-docstring (org-parent-node) ())

(defmethod node-dispatchers :around ((node lisp-docstring))
  (mapcar #'make-instance '(lisp-docstring-section-node)))

(defclass lisp-docstring-section-node (lisp-docstring) 
  ((heading :accessor node.heading :initform nil :initarg :heading)))

(defun section-node-start-p (stack)
  (when (and (eql #\Newline (first stack))
	     (eql #\Newline (second stack))
	     (eql (third stack) #\:))
    (break "~A second stack" (char-name  (Second stack)))
    
    (let ((stack (cddr stack))) 
      (and (eql (first stack) #\:)
	   (loop 
	      :for (char . chars) on stack
	      :collect char into heading
	      :if (and (or (null chars)
			   (and (eql (first chars) #\Newline)
				(eql (second chars) #\Newline)))
		       (alpha-char-p char))
	      :do (return (values heading chars)))))))

(defmethod node-start ((node lisp-docstring-section-node) stack)
  (multiple-value-bind (heading old-stack) 
      (section-node-start-p stack)
    (if heading 
	(values  
	 (make-instance (class-of node) :heading (stack->string heading)) 
	   old-stack))))

(defmethod node-end ((node lisp-docstring-section-node) (next-node lisp-docstring-section-node) stack)   
  t)

(defmethod node-end ((node lisp-docstring-section-node) (next-node null) stack)   
  t)

(defmethod print-node ((node lisp-docstring-section-node) &optional stream)
  (format stream "*~A*~%~%"(node.heading node))
  (call-next-method))

(defun parse-docstring (string)
  (with-input-from-string (stream string) 
    (read-parent-node (make-instance 'lisp-docstring) stream)))(in-package :cl-org-mode)

(defun print-docstring (name type stream)
  (let ((docstring (parse-docstring (documentation name type))))
    (if (typep (first (node.children docstring)) 'lisp-docstring-section-node)
	(print-node docstring stream)
	(progn (format stream "*Description:*~%")
	       (print-node docstring stream)))))

(defun print-doc-title (object name stream)
  (format stream "/~A/ "   
	  (split-sequence:split-sequence #\- (princ-to-string (type-of object))))
  (format stream "=~a="   
	  name))

(defun newline-and-indent (stream depth &optional (num 1)  (char #\Space) (newline #'terpri))
  (dotimes (n num) (funcall newline stream)
	   (dotimes (n depth)
	     (princ char stream))
	   (princ #\Space stream)))

(defun cl-user::%t (stream arg colon at &rest parameters)
  (if (listp arg)
      (destructuring-bind (depth char) arg
	(newline-and-indent stream depth (or (first parameters) 1) char (if colon #'fresh-line #'terpri)))
      (newline-and-indent stream arg (or (first parameters) 1))))

(defun convert-newline-to-indent (string indent-level output-stream)
  (map nil (lambda (char) (if (eql char #\newline)
			      (format output-stream "~/%t/" indent-level)
			      (princ char output-stream)))
       string))
(defun print-lisp-documentation-to-org-node (spec stream &key (starting-depth 2))
  (flet ((p ()
	   (format stream "~2/%t/" starting-depth)) 
	 (print-doc-title (object name)
	   (format stream "~:/%t/~@?~2/%t/~4:*~@?" 		 
		   `(,starting-depth #\*)
		   "/~:(~{~A~^ ~}~)/ =~A="
		   (split-sequence:split-sequence  #\- (princ-to-string (type-of object)))
		   name
		   starting-depth))
	 (print-function-syntax (name)
	   (format stream "*Syntax:*~2/%t/=~(~A~)= " 
		   starting-depth 
		   name)
	   (dolist (arg (swank::arglist name))
	     (and (listp arg) (setf arg (car arg)))
	     (format stream
		     (if (and (symbolp arg) 
			      (eql (aref (symbol-name arg) 0)
				   #\&))
			 "/~(~A~)/ "
			 "~(~A~) ") 
		     arg) )
	   (format stream "=> /result/")))


    (if (listp spec)
	(destructuring-bind (type name) spec
	  (case type
	    (function 
	     (let ((function (eval `#',name)))
	       (print-doc-title function name)
	       (p)
	       (print-function-syntax name)
	       (p)
	       (convert-newline-to-indent 
		(with-output-to-string (s) (print-docstring name type s))
		starting-depth 
		stream))))
	  ))))