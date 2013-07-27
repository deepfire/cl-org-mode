(in-package :cl-org-mode)

(defclass org-node (node) ())

(defmethod node-start ((node org-node) stream) nil)

(defmethod node-end ((node org-node) next-node stream) nil)

(defmethod node-end ((node org-node) (next-node null) stack)
  T)

(defmethod node-dispatchers ((node org-node))
  (or *dispatchers* 
      (mapcar #'make-instance '(src-node properties-node outline-node))))

(defmethod node-prototypes (node)
  (error "never call"))


(defmethod finalize-node (node next-node stack)
  "If there is something on the stack, and a new node has started,
then stick it in the default node"
;  (break "Finalizing ~A ~A ~A ~A" node next-node stack (node.next-node node))
  (setf (node.next-node node) 
	(if stack
	    (make-default-node node next-node stack)
	  next-node)))

(defgeneric make-default-node (node next-node stack)
  (:documentation "Anything not in another node ends up here")
  (:method ((node org-node) next-node stack)
;  (break "Making default: ~A ~A ~A" node next-node stack)
	   (make-instance 'text-node 
			  :text (stack->string stack)
			  :next-node next-node)))

(defclass text-node (org-node)
  ((text :initarg :text :accessor node.text)))(in-package :cl-org-mode)

(defclass org-parent-node (org-node)
  ((child-nodes :initarg :children 
		:accessor node.children
 		:initform nil)
   (include-end-node :initarg :include-end-node
		     :initform nil
		     :reader include-end-node-p))
  (:documentation "Some node contain other nodes"))

(defun read-child-nodes (root-node stack stream )
  (loop 
     :for next-node = (read-node root-node stream) 
     :then (and next-node (read-node next-node stream))
     :until (node-end root-node next-node stack)
     ;:do (warn "reading root ~A : next-node : ~A ~A" root-node next-node stack)
     :collect next-node into nodes
     :finally (return (if (include-end-node-p root-node)
			  (values (nconc nodes (list next-node)) 
				  next-node) 
			  (values  nodes next-node)))))
  

(defmethod read-next-node ((node org-parent-node)  (next-node null) stream)
  (call-next-method)

  (multiple-value-bind (children new-node) 
      (read-child-nodes node nil stream)
    (when children 
      (unless (eql (car children) (node.next-node node))
	;;; Somebody short-circuited the process, namely property-node. why?
	(setf children (cons (node.next-node node) children))
	))

    (setf (node.children node) children)
    new-node))

(defmethod read-node :around ((node org-parent-node) stream)
 (let ((new-node (call-next-method)))
   (if (include-end-node-p node)
       (read-node new-node stream)
     new-node)))

(defun read-parent-node (parent-node stream)
  (read-node parent-node stream) parent-node) 
    
(defclass org-file (org-parent-node) 
  ((pathname :accessor node.pathname :initarg :pathname)))

(defun read-org-file (pathname)
  (let ((node (make-instance 'org-file :pathname pathname)))
    (setf node (megaparser (alexandria:read-file-into-string pathname)))
    node))

(defclass delimited-node (org-parent-node)
  ((opening-delimiter :initarg :opening-delimiter :accessor node.opening-delimiter :initform nil)
   (closing-delimiter :initarg :closing-delimiter :accessor node.closing-delimiter :initform nil)
   (closing-delimiter-node :accessor node.closing-delimiter-node)
   (node-closed :initform nil :accessor node.closed-p))

  (:default-initargs :include-end-node t))

(defclass closing-delimiter-node (org-node)
 ((opening-delimiter-node :accessor node.opening-delimiter-node :initarg :opening-delimiter-node)))

(defmethod shared-initialize :after ((node delimited-node) slots &rest args)
  (declare (ignore args))
  (setf (node.closing-delimiter-node node)
	(make-instance 'closing-delimiter-node :opening-delimiter-node node)))

(defmethod node-dispatchers ((node delimited-node))
  (if (node.closed-p node)
      (call-next-method)
      (cons (node.closing-delimiter-node node) (call-next-method))))

(defmethod node-start ((node delimited-node) stack)
  (with-slots (opening-delimiter) node
    (when opening-delimiter 
      (multiple-value-bind (delimiter old-stack)
	  (stack-starts-with stack opening-delimiter)
	
	(when delimiter
	  (values (make-instance (class-of node) 
				 :opening-delimiter (stack->string delimiter)) 
		   old-stack))))))

(defmethod node-start ((node closing-delimiter-node) stack)

  (with-slots (closing-delimiter) (node.opening-delimiter-node node)
    (multiple-value-bind (indicator old-stack)
	(stack-starts-with stack closing-delimiter)
      (when indicator 
	(values node 
		old-stack)))))

(defmethod node-end ((node delimited-node) (next-node closing-delimiter-node) stack)
  (when (eq next-node (node.closing-delimiter-node node))
    (setf (node.closed-p node) t)))

(defmethod read-next-node :around ((node delimited-node) next-node stream)
  (call-next-method)
)(in-package :cl-org-mode)

(defclass outline-node (org-parent-node) 
  ((heading :accessor node.heading :initform nil :initarg :heading)
   (heading-level-indicator :accessor node.heading-level-indicator
			    :initform nil
			    :initarg :indicator)))

(defun at-outline-node-p (stack)
  (let ((char (first stack))
	(stack (rest stack)))
    (and (eql char #\space)
	 (eql (first stack) #\*)
	 (if (or (null (rest stack))
		 (eql #\Newline (second stack)))
	     (values t  (rest stack))
	     (at-outline-node-p (cons char (rest stack)))))))

(defmethod node-start ((node outline-node) stack)
  (multiple-value-bind (pred old-stack) 
      (at-outline-node-p stack)
    (if pred 
	(values  
	   (make-instance (class-of node) 
			  :indicator
			  (loop 
			     for cons on stack 
			     until (eq cons old-stack) 
			     collect (car cons))) 
	   old-stack))))

(defmethod node-end ((node outline-node) (next-node outline-node) stack)   
  (<= (length (node.heading-level-indicator next-node))
      (length (node.heading-level-indicator node))))

(defmethod node-end ((node outline-node) (next-node null) stack)   
  t)

(defmethod read-next-node ((node outline-node) (next-node null) stream)
  (setf (node.heading node) (read-line stream nil))
  (call-next-method))(in-package :cl-org-mode)

(defclass src-node (delimited-node text-node)
  ((emacs-mode :initarg :emacs-mode :accessor node.emacs-mode :initform nil))
  (:default-initargs 
    :opening-delimiter "#+BEGIN_SRC"
    :closing-delimiter (format nil "~%#+END_SRC")
    :text nil
    :include-end-node nil))


(defmethod node-dispatchers ((node src-node))
  (if (node.text node)
      (call-next-method)
      (list (node.closing-delimiter-node node))))

(defmethod read-next-node ((node src-node) (next-node null) stream)
  (setf (node.emacs-mode node) (read-line stream nil))
  (call-next-method))

(defmethod finalize-node ((node src-node) next-node stack)
  (setf (node.next-node node) next-node)
   (setf (node.text node) (stack->string stack))
  next-node)(in-package :cl-org-mode)

(defclass properties-node (delimited-node)
  ()

  (:default-initargs 
    :opening-delimiter ":PROPERTIES:
"
    :closing-delimiter ":END:"))

(defmethod finalize-node ((node properties-node) next-node stack)
  (call-next-method))

(defclass property-node (delimited-node)
  ((property :initarg :property :accessor property-node.property)
   (value :initarg :value :accessor property-node.value))  
  (:default-initargs 
    :closing-delimiter (format nil "~%")))

(defmethod node-start ((node property-node) stack)
  (let ((pos (position #\: (cdr stack))))
    (when (and pos (eql #\: (car stack)))

      (let ((property (nreverse (coerce (subseq (cdr stack) 0 pos) 'string ))))
	(when property
	  (values (make-instance (class-of node)
				 :property property)
		  (subseq (cddr stack) pos)))))))


(defmethod finalize-node ((node property-node) next-node stack)
  (setf (property-node.value node) (nreverse (coerce (butlast stack) 'string)))
  (call-next-method node next-node (cons (first stack ) (last stack))))

(defmethod node-dispatchers ((node properties-node))
  (let ((dispatchers (call-next-method)))
    (list (node.closing-delimiter-node node)
	   (make-instance 'property-node)
	   )))

(defun get-property-value (node key)
  (let ((node (find-if 
	       (lambda (n)
		 (and (typep n 'property-node)
		      (equal (property-node.property n) key)))
	       (node.children node))))
    (when node (property-node.value node))))
