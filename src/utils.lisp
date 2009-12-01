(in-package :cl-org-mode)

(defun stack->string (stack)
  (nreverse (coerce stack 'string)))

(defgeneric stack-starts-with (stack maybe-starts-with)
  (:documentation 
   "return (values start-of-stack rest-of-stack) if stack starts with maybe-starts-with.
If maybe-starts-with is a string. reverse it before testing against stack")
  (:method (stack list)
    ;; there are better ways to do this i'm sure.
    (let ((does-it? (when (>= (length stack)(length list))
		      (loop :for cons on stack 
			 :for pair on list
			 :always (eql (car cons) (first pair))))))
		      
      (when does-it? 
	(values list (nthcdr (length list) stack)))))
  
  (:method (stack (string string))
    (stack-starts-with stack (coerce (reverse string) 'list))))