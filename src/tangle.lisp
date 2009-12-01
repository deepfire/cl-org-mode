(in-package :cl-org-mode)

(defun tangle-org-node (node &key (source-directory (node.pathname node)) source-file)
  "Arguments and Values:

node---an ORG-NODE
source-directory---a pathname,
source-file---a pathname.
result---returns NIL. 

Description:

Walk node and all its children, and print the contents of
any SRC-NODE to source-file. If source-file is a relative pathname, it
will be merged with source-directory.

If node or any of its children contain :source-file:
or :source-directory: properties, they will replace the value of
source-file or source-directory for that node and its children."

  (flet ((tangle () (loop for child in (node.children node)
	      :do  
	      (typecase child 
				(outline-node 
		 (let ((properties (find-if (lambda (x) (typep x 'properties-node))
					    (node.children child))))

		   (when properties 
		     (setf source-directory 
			   (let ((path (get-property-value properties "source-directory")))
			     (if path
				 (if (eq :RELATIVE (first (pathname-directory path)))
				     (merge-pathnames path source-directory)
				     path)
				 source-directory)))
		     (setf source-file (or (get-property-value properties "source-file") source-file)))
		   (tangle-org-node child 
				    :source-directory source-directory
				    :source-file source-file
				    )))
		(src-node
		 (if (streamp source-file)
		     (write-sequence (node.text child) source-file)
		     (warn "Source node has no stream")))))))
	 (if (and source-file
		  (not (streamp source-file)))
	     (alexandria:with-output-to-file (stream (merge-pathnames source-file source-directory) 
						     :if-exists :supersede 
						     :if-does-not-exist :create)
	       (setf source-file stream)
	       (tangle))
	     (tangle))))