(in-package :cl-org-mode)

;;;
;;; Tools
(defun indent (x stream)
  (dotimes (i x)
    (write-char #\Space stream)))

(defun org-present-properties (properties indent stream)
  (indent indent stream)
  (write-line ":PROPERTIES:" stream)
  (iter (for (name . value) in properties)
        (indent indent stream)
        (format stream ":~A:~:[~; ~:*~A~]~%" name value))
  (indent indent stream)
  (write-line ":END:" stream))

;;;
;;; Methods
(defvar *present-depth*)
(defvar *present-depth-increment*)
(defvar *presented-nodes*)

(defmethod org-present (kind (o string) s)
  (write-string o s))

(defun mark-node-presented (node)
  (setf (gethash node *presented-nodes*) t))

(defun node-presented-p (node)
  (gethash node *presented-nodes*))

(defmethod org-present (kind (o org-document) s)
  (with-slots (options title section) o
    (iter (for (option value . rest) on options by #'cddr)
          (indent *present-depth* s)
          (format s "#+~A:" (symbol-name option))
          (if (eq option :startup)
              (format s "~{ ~A~}" (mapcar #'car (plist-alist value)))
              (format s " ~A" value))
          (terpri s))
    (indent *present-depth* s)
    (format s "#+TITLE: ~A~%" title)
    (when-let ((properties (properties-of o)))
      (org-present-properties properties *present-depth* s))
    (org-present kind section s)
    (let ((*presented-nodes* (make-hash-table :test 'eq)))
      (mark-node-presented o)
      (dolist (c (node.out o))
        (let ((*present-depth* (+ *present-depth-increment* *present-depth*)))
          (unless (node-presented-p c)
            (org-present kind c s)))))))

(defmethod org-present ((kind (eql :flat)) (o org-document) s)
  (let ((*present-depth* 0)
        (*present-depth-increment* 0))
    (call-next-method)))

(defmethod org-present ((kind (eql :normal)) (o org-document) s)
  (let ((*present-depth* -2)
        (*present-depth-increment* 2))
    (call-next-method)))

(defmethod org-present (kind (o org-node) s)
  (mark-node-presented o)
  (with-slots (status priority title tags section) o
    (dotimes (i *present-depth*)
      (write-char #\* s))
    (format s "*~:[~; ~:*~A~]~:[~; ~:*~A~] ~A~:[~; ~:*:~{~A:~}~]~%"
            status priority title tags)
    (when-let ((properties (properties-of o)))
      (org-present-properties properties *present-depth* s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (let ((*present-depth* (+ *present-depth-increment* *present-depth*)))
        (unless (node-presented-p c)
          (org-present :flat c s))))))

(defmethod org-present (kind (o org-container) s)
  (dolist (c (children-of o))
    (org-present :flat c s)))

(defmethod org-present (kind (o org-properties) s)
  (indent *present-depth* s)
  (format s ":PROPERTIES:~%")
  (call-next-method)
  (indent *present-depth* s)
  (format s ":END:~%"))

(defmethod org-present (kind (o org-drawer) s)
  (with-slots (name) o
    (indent *present-depth* s)
    (format s ":~A:~%" name)
    (call-next-method)
    (indent *present-depth* s)
    (format s ":END:~%")))

(defmethod org-present (kind (o org-block) s)
  (with-slots (name parameters) o
    (indent *present-depth* s)
    (format s "#+BEGIN_~A:~:[~;~:* ~A~]~%" name parameters)
    (call-next-method)
    (indent *present-depth* s)
    (format s "#+END_~A~%" name)))

(defmethod org-present (kind (o org-keyword) s)
  (indent *present-depth* s)
  (format s "#+~A:~:[~;~:* [~A]~] ~A~%" (name-of o) (optional-of o) (value-of o)))
