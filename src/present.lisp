(in-package :cl-org-mode)

;;;
;;; Tools
(defun org-present-flat-properties (properties stream)
  (write-line ":PROPERTIES:" stream)
  (iter (for (name . value) in properties)
        (format stream ":~A:~:[~; ~:*~A~]~%" name value))
  (write-line ":END:" stream))

;;;
;;; Methods
(defmethod org-present ((kind (eql :flat)) (o string) s)
  (write-string o s))

(defmethod org-present ((kind (eql :flat)) (o org-document) s)
  (with-slots (options title section) o
    (iter (for (option value . rest) on options by #'cddr)
          (format s "#+~A: ~A~%" (symbol-name option) value))
    (format s "#+TITLE: ~A~%" title)
    (when-let ((properties (properties-of o)))
      (org-present-flat-properties properties s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (org-present :flat c s))))

(defmethod org-present ((kind (eql :flat)) (o org-node) s)
  (with-slots (status priority title tags section) o
    (format s "*~:[~; ~:*~A~]~:[~; ~:*~A~] ~A~:[~; ~:*~A~]~%"
            status priority title tags)
    (when-let ((properties (properties-of o)))
      (org-present-flat-properties properties s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (org-present :flat c s))))

(defmethod org-present ((kind (eql :flat)) (o org-container) s)
  (dolist (c (children-of o))
    (org-present :flat c s)))

(defmethod org-present ((kind (eql :flat)) (o org-properties) s)
  (format s ":PROPERTIES:~%")
  (call-next-method)
  (format s ":END:~%"))

(defmethod org-present ((kind (eql :flat)) (o org-drawer) s)
  (with-slots (name) o
    (format s ":~A:~%" name)
    (call-next-method)
    (format s ":END:~%")))

(defmethod org-present ((kind (eql :flat)) (o org-block) s)
  (with-slots (name parameters) o
    (format s "#+BEGIN_~A:~:[~;~:* ~A~]~%" name parameters)
    (call-next-method)
    (format s "#+END_~A~%" name)))

(defmethod org-present ((kind (eql :flat)) (o org-keyword) s)
  (format s "#+~A:~:[~;~:* [~A]~] ~A~%" (name-of o) (optional-of o) (value-of o)))
