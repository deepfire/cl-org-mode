(in-package :cl-org-mode)

(defgeneric print-node (node &optional stream)
  (:documentation "Print text serialization of node.")
  (:method :around (node &optional (stream *standard-output*))
	   (call-next-method node stream)))

(defmethod print-node :around ((node delimited-node) &optional stream)
  (write-sequence (node.opening-delimiter node) stream)
  (call-next-method))

(defmethod print-node ((node closing-delimiter-node) &optional stream)
  (princ (node.closing-delimiter (node.opening-delimiter-node node)) stream))

(defmethod print-node :around ((node outline-node) &optional stream)
  (format stream "~A~A~%" 
	  (stack->string  (node.heading-level-indicator node))
	  (node.heading node)) 
  (call-next-method))

(defmethod print-node ((node property-node) &optional stream)
  (with-slots (property value) node
    (format stream ":~A: ~A~%" property value)))

(defmethod print-node  ((node src-node) &optional stream)
  (format stream "~A~%"(node.emacs-mode node))
  (write-sequence (node.text node) stream))