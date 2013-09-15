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

(defmethod org-present (kind (o string) s)
  (write-string o s))

(defmethod org-present (kind (o org-document) s)
  (with-slots (options title section) o
    (iter (for (option value . rest) on options by #'cddr)
          (indent *present-depth* s)
          (format s "#+~A: ~A~%" (symbol-name option) value))
    (indent *present-depth* s)
    (format s "#+TITLE: ~A~%" title)
    (when-let ((properties (properties-of o)))
      (org-present-properties properties *present-depth* s))
    (org-present kind section s)
    (dolist (c (node.out o))
      (let ((*present-depth* (+ *present-depth-increment* *present-depth*)))
        (org-present kind c s)))))

(defmethod org-present ((kind (eql :flat)) (o org-document) s)
  (let ((*present-depth* 0)
        (*present-depth-increment* 0))
    (call-next-method)))

(defmethod org-present ((kind (eql :normal)) (o org-document) s)
  (let ((*present-depth* -2)
        (*present-depth-increment* 2))
    (call-next-method)))

(defmethod org-present (kind (o org-node) s)
  (with-slots (status priority title tags section) o
    (dotimes (i *present-depth*)
      (write-char #\* s))
    (format s "*~:[~; ~:*~A~]~:[~; ~:*~A~] ~A~:[~; ~:*~A~]~%"
            status priority title tags)
    (when-let ((properties (properties-of o)))
      (org-present-properties properties *present-depth* s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (let ((*present-depth* (+ *present-depth-increment* *present-depth*)))
        (org-present :flat c s)))))

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
