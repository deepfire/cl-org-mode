(in-package :cl-org-mode-extended)

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

(defun clear-hash-cache (&optional (cache *hash-cache*))
  (with-slots (node->hash hash->node) cache
    (clrhash hash->node)
    (clrhash node->hash)))

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
