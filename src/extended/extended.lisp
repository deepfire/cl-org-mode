(in-package :cl-org-mode-extended)

;;;
;;; Properties
(define-constant +child-property-prefix+ "CL-ORG-MODE-CHILD-" :test 'equal)

(defun children-property-p (cons)
  (starts-with-subseq +child-property-prefix+ (car cons)))

(defmethod dynamic-properties-of ((o org-node) (kind (eql :children)))
  (iter (for i from 0)
        (for c in (node.out o))
        (collect (cons (strconcat* +child-property-prefix+ (write-to-string i :base 10))
                       (write-to-string (hash-of c) :base #x10)))))

(defmethod dynamic-property-p ((o cons))
  (children-property-p o))

(defmethod properties-of ((o org-node))
  (append (remove-if #'dynamic-property-p (static-properties-of o))
          (dynamic-properties-of o :children)))

(defmethod org-present :around (kind (o org-document) stream)
  (with-hash-cache ()
    (call-next-method)))

;;;
;;; Twilight zone: operation before the DAG is restored
(defun node-static-property-children-hashed-p (node)
  "All static-property-referenced children are present in the hash cache."
  (iter (for prop in (static-properties-of node))
        (always (or (not (children-property-p prop))
                    (destructuring-bind (name . hash-string) prop
                      (declare (ignore name))
                      (node (parse-integer hash-string :radix #x10)))))))

(defun node-children-hashed-p (node)
  "A sufficient (but not necessary) condition for adequacy of HASH-OF."
  ;; Two partial pictures get merged here -- org-property-based and org-physical.
  (and (node-static-property-children-hashed-p node)
       (every #'node-hashed-p (node.out node))))

;;;
;;; The restorer
(defvar *debug-redag* nil)

(defmacro with-redag-debug ((&key (enable t)) &body body)
  `(let ((*debug-redag* ,enable))
     ,@body))

(defun org-dress-document-extended (root)
  "Recover supra-org, cl-org-mode -specific information, as encoded in properties."
  (labels ((node-property-children (node child-prop-ptrs)
             (iter (for i from 0)
                   (for (name . value) in (sort child-prop-ptrs #'string< :key #'car))
                   (let ((prop-index (when (and (stringp name)
                                                (plusp (length name)))
                                       (parse-integer name :start (length +child-property-prefix+) :junk-allowed t))))
                     (unless (eql i prop-index)
                       (org-object-error "~@<Inconsistent child properties in ~S: ~D'th property (~S) has id ~D.~:@>" node i name prop-index)))
                   (collect (node (parse-integer value :radix #x10)))))
           (fixup-children-links (node)
             (multiple-value-bind (child-prop-ptrs rest)
                 (unzip #'dynamic-property-p (static-properties-of node))
               (when *debug-redag*
                 (format t "~S~%" node)
                 (format t "   pre-node.out   ~S~%" (node.out node))
                 (format t "   stat-props     ~S~%" (static-properties-of node)))
               (let* ((property-referenced-children (node-property-children node child-prop-ptrs))
                      (statically-unlinked-children (set-difference property-referenced-children (node.out node)))
                      (property-unlinked-children (set-difference (node.out node) property-referenced-children)))
                 (when *debug-redag*
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
                                       node property-unlinked-children)))))
           (restore-children-links-from-properties (root)
             (let* ((seen (make-hashset nil :test 'eq))
                    (progress (make-hash-table :test 'eq))
                    ;; The frontier stores nodes, whose hashability is unknown, yet reasonably plausible,
                    ;; i.e. that they have at least one hashed child.
                    (frontier (lret ((acc (make-queue)))
                                ;; At this point "leafness" still only means org-physical leafness..
                                (do-leaf-nodes (n root)
                                  ;; ..so we need to guard against nodes that are not leaves in the org-property sense:
                                  (unless (some #'children-property-p (static-properties-of n))
                                    (hash-of n)
                                    (hashset-add n seen)
                                    ;; The initial frontier contains parents of leaf nodes,
                                    ;; as per above definition.
                                    (enqueue-list (node.in n) acc))))))
               ;; Unlinked nodes lead to non-termination here.
               (iter (for n = (dequeue frontier))
                     (for i from 0)
                     (while (not (queue-empty-p frontier)))
                     (when (in-hashset-p n seen)
                       (org-object-error "~@<DAG circularity violation: ~S is a member of a loop.~@:>" n))
                     (cond
                       ((node-children-hashed-p n)
                        (fixup-children-links n)
                        (hash-of n)
                        (hashset-add n seen)
                        (enqueue-list (node.in n) frontier))
                       (t
                        ;; Don't perform this check too often (note that the
                        ;; approximation here converges in a cubic manner):
                        (when (zerop (mod i 100))
                          (let ((current-progress (hash-cache-size)))
                            (when-let ((old-progress-at-this-point (gethash n progress)))
                              (when (>= old-progress-at-this-point current-progress)
                                (org-object-error "~@<DAG restorer stuck with ~D unhashable nodes remaining: ~S.~:@>"
                                                  (length (queue-contents frontier)) (queue-contents frontier))))
                            (setf (gethash n progress) current-progress)))
                        (enqueue n frontier)))))))
    (restore-children-links-from-properties root)))

(defun org-parse-extended (org)
  (let ((doc (org-parse org)))
    (with-hash-cache ()
      (org-dress-document-extended doc)
      doc)))
