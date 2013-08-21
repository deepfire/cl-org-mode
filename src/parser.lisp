(in-package :cl-org-mode)

(defmethod read-next-node (node (next-node null) stream) 
  "This method is called when we don't yet know what the next node is"
  (let (stack)
    (loop for char = (read-char stream nil)
       :if (null char) 
       :do (return (finalize-node node NIL stack))
       :else 
       :do (push char stack)
	 (multiple-value-bind (new-node old-stack)
		 (find-next-node node next-node stack)	   
	       (when new-node 
		 (return (finalize-node node new-node old-stack)))))))

(defmethod read-next-node (node (next-node node) stream)
  "When we know what the node is already, just return it"
  next-node)

(defmethod read-next-node :around (node next-node stream)
  (let ((*dispatchers* (node-dispatchers node)))
  ;(warn "DISPATHERS FOR ~A ~A: ~A" node next-node *dispatchers*)
    (call-next-method)))


(defmethod find-next-node (node next-node stack)
  (loop for object in *dispatchers*
       :do (multiple-value-bind (result old-stack)
	       (node-start object stack)
	     (when result (return (values result old-stack))))))
