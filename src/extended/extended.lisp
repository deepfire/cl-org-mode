(in-package :cl-org-mode-extended)

;;;
;;; Conditions
(define-condition org-dress-error (org-error)
  ())

(define-condition org-object-error (org-error)
  ((hash :reader hash-of :initarg :hash)))

(define-condition org-object-warning (org-warning)
  ((hash :reader hash-of :initarg :hash)))

(cl-org-mode::define-simple-error   org-object-error)
(cl-org-mode::define-simple-warning org-object-warning)

(define-condition org-object-relink-error (org-dress-error)
  ())

;;;
;;; Hashing
(defparameter *debug-hash* nil)

(defmacro with-hash-debug (() &body body)
  `(let ((*debug-hash* 0))
     ,@body))

(defun print-spaces (n)
  (dotimes (i n)
    (write-char #\Space *debug-io*)))

(defmethod hash-of ((o org-node))
  (let ((*package* (find-package :common-lisp))
        (*print-base* #x10))
    (ironclad:octets-to-integer
     (ironclad:with-digesting-text-stream (s :sha256 :external-format :utf-8)
       (labels ((write-tag-to (stream name &rest xs)
                  (declare (string name))
                  (write-string name stream)
                  (dolist (x xs)
                    (write-char #\Space stream)
                    (prin1 x stream))
                  (terpri stream))
                (write-tag (name &rest xs)
                  (when *debug-hash*
                    (print-spaces *debug-hash*)
                    (apply #'write-tag-to *debug-io* name xs))
                  (apply #'write-tag-to s name xs)))
         (with-slots (title section out) o
           (declare (string title)
                    ((or null org-section) section)
                    (list out))
           (write-tag "title" title)
           (let ((*debug-hash* (when *debug-hash*
                                 (+ 4 *debug-hash*))))
             (iter (for (name . value) in (properties-of o))
                   (declare (string name)
                            ((or null string) value))
                   (write-tag "prop" name value))
             (when section
               (write-tag "sectn" (hash-of section)))
             (when out
               (dolist (c out)
                 (with-slots (status priority tags) c
                   (declare ((or null string) status priority)
                            (list tags))
                   (assert (every #'stringp tags))
                   (let ((child-desc (format nil " ~X~:[~;~:* stat ~S~]~:[~;~:* prio ~S~]~:[~;~:* tags~{ ~S~}~]~%"
                                             (hash-of c) status priority tags)))
                     (when *debug-hash*
                       (print-spaces *debug-hash*)
                       (format *debug-io* "child ~S~A" (title-of c) child-desc))
                     (write-string "child" s)
                     (write-string child-desc s))))))))))))

(defmethod hash-of ((o org-section))
  (let ((*package* (find-package :common-lisp))
        (*print-base* #x10))
    (ironclad:octets-to-integer
     (ironclad:with-digesting-text-stream (s :sha256 :external-format :utf-8)
       (org-present :flat o s)))))

;;;
;;; Hash cache
(defclass hash-cache ()
  ((hash->node :initarg :hash->node)
   (node->hash :initarg :node->hash))
  (:default-initargs
   :hash->node (make-hash-table)
   :node->hash (make-hash-table :test 'eq)))

(defvar *hash-cache*)

(defun call-with-hash-cache (force fn)
  (if (and (boundp '*hash-cache*)
           (not force))
      (funcall fn)
      (let ((*hash-cache* (make-instance 'hash-cache)))
        (funcall fn))))

(defmacro with-hash-cache ((&key force) &body body)
  `(call-with-hash-cache ,force (lambda () ,@body)))

(defun add-to-hash-cache (cache node hash)
  (declare (hash-cache cache)
           (org-node node)
           (integer hash))
  (with-slots (node->hash hash->node) cache
    (when (nth-value 1 (gethash node node->hash))
      (org-object-error "~@<Hash conflict: while inserting hash ~S -- node ~S already occupied by hash ~S.~:@>"
                        hash node (gethash node node->hash)))
    (when (nth-value 1 (gethash hash hash->node))
      (org-object-error "~@<Hash conflict: while inserting node ~S -- hash ~S already occupied by node ~S.~:@>"
                        node hash (gethash hash hash->node)))
    (setf (gethash node node->hash) hash
          (gethash hash hash->node) node))
  hash)

(defmethod hash-of :around ((o org-node))
  (let ((cache *hash-cache*))
    (or (gethash o (slot-value cache 'node->hash))
        (add-to-hash-cache cache o (call-next-method)))))

(defun node (hash)
  (declare (integer hash))
  (gethash hash (slot-value *hash-cache* 'hash->node)))

(defun node-hashed-p (node)
  (nth-value 1 (gethash node (slot-value *hash-cache* 'node->hash))))

(defun invalidate-node-hash (node)
  (when (node-hashed-p node)
    (let ((hash (hash-of node)))
      (remhash hash (slot-value *hash-cache* 'hash->node))
      (remhash node (slot-value *hash-cache* 'node->hash)))))

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
           (restore-children-dag (node)
             (multiple-value-bind (child-prop-ptrs rest)
                 (unzip (curry #'starts-with-subseq +child-property-prefix+)
                        (static-properties-of node) :key #'car)
               (setf (slot-value node 'static-properties) rest)
               (let* ((property-referenced-children (node-property-children node child-prop-ptrs))
                      (unlinked-children (set-difference property-referenced-children (node.out node)))
                      (unpropertied-children (set-difference (node.out node) property-referenced-children)))
                 (dolist (c unlinked-children)
                   (push node (node.in c)))
                 (cond
                   (unpropertied-children
                    (org-object-warning "~@<Node ~S: incomplete child properties: following children have no corresponding properties:~{ ~S~}.~:@>"
                                        node unpropertied-children)
                    (appendf (node.out node) unlinked-children))
                   (t
                    (setf (node.out node) property-referenced-children)))))))
    (restore-children-dag node)))

(defun org-parse-extended (org)
  (let ((doc (org-parse org)))
    (with-hash-cache ()
      ;; populate the hash cache for every node
      (hash-of doc)
      (org-dress-node-extended doc)
      doc)))
