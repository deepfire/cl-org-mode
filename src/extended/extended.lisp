(in-package :cl-org-mode-extended)

;;;
;;; Properties
(define-constant +child-property-prefix+ "CL-ORG-MODE-CHILD-" :test 'equal)

(defmethod properties-of ((o org-node))
  (append
   (static-properties-of o)
   (dynamic-properties-of o :children)))

(defmethod dynamic-properties-of ((o org-node) (kind (eql :children)))
  (iter (for i from 0)
        (for c in (node.out o))
        (collect (cons (strconcat* +child-property-prefix+ (write-to-string i :base 10))
                       (write-to-string (hash-of c) :base #x10)))))

(defmethod org-present :around (kind (o org-document) stream)
  (with-hash-cache ()
    (call-next-method)))

(defvar *debug-redag* nil)

(defmacro with-redag-debug (() &body body)
  `(let ((*debug-redag* t))
     ,@body))

;;;
;;; The restorer
(defun org-dress-node-extended (node)
  "Recover supra-org, cl-org-mode -specific information, as encoded in properties."
  (labels ((node-property-children (node child-prop-ptrs)
             (iter (for i from 0)
                   (for (name . value) in (sort child-prop-ptrs #'string< :key #'car))
                   (let ((prop-index (when (and (stringp name)
                                                (plusp (length name)))
                                       (parse-integer name :start (length +child-property-prefix+) :junk-allowed t))))
                     (unless (eql i prop-index)
                       (org-object-error "~@<Inconsistent child properties in ~S: ~D'th property (~S) has id ~D.~:@>" node i name prop-index)))
                   (let* ((hash (parse-integer value :radix #x10))
                          (child (node hash)))
                     (unless child
                       (org-object-error "~@<Node ~S: missing child number ~S, hash ~S.~:@>" node i hash))
                     (collect child))))
           (restore-children-links-from-properties (node)
             (multiple-value-bind (child-prop-ptrs rest)
                 (unzip (curry #'starts-with-subseq +child-property-prefix+)
                        (static-properties-of node) :key #'car)
               (let* ((property-referenced-children (node-property-children node child-prop-ptrs))
                      (statically-unlinked-children (set-difference property-referenced-children (node.out node)))
                      (property-unlinked-children (set-difference (node.out node) property-referenced-children)))
                 (when *debug-redag*
                   (format t "~S~%" node)
                   (format t "   pre-node.out   ~S~%" (node.out node))
                   (format t "   stat-props     ~S~%" (static-properties-of node))
                   (format t "   new-stat-prop  ~S~%" rest)
                   (format t "   prop-ref-chi   ~S~%" property-referenced-children)
                   (format t "   prop-only-chi  ~S~%" statically-unlinked-children)
                   (format t "   prop-lack-chi  ~S~%" property-unlinked-children))
                 (setf (slot-value node 'static-properties) rest)
                 (dolist (c statically-unlinked-children)
                   (push node (node.in c))
                   (push c (node.out node)))
                 (when property-unlinked-children
                   (org-object-warning "~@<Node ~S: incomplete child properties: following children have no corresponding properties:~{ ~S~}.~:@>"
                                       node property-unlinked-children))
                 (dolist (c (node.out node))
                   (restore-children-links-from-properties c))))))
    (restore-children-links-from-properties node)))

(defun org-parse-extended (org)
  (let ((doc (org-parse org)))
    (with-hash-cache ()
      ;; populate the hash cache for every node
      (hash-of doc)
      (org-dress-node-extended doc)
      (clear-hash-cache)
      (hash-of doc)
      doc)))
