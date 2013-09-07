(in-package :cl-org-mode)

(define-condition org-error (error)
  ())

(define-condition org-parse-error (org-error)
  ((source   :reader source-of   :initarg :source)
   (line     :reader line-of     :initarg :line)
   (column   :reader column-of   :initarg :column)))

(defclass org-node (node)
  ((title      :reader title-of      :initarg :title)
   (section    :reader section-of    :initarg :section)
   (status     :reader status-of     :initarg :status)
   (priority   :reader priority-of   :initarg :priority)
   (tags       :reader tags-of       :initarg :tags)
   (properties :reader properties-of :initarg :properties)))

(defclass org-document (org-node)
  ())

(defun make-node (title section children &key status priority tags properties
                                           (type 'org-node))
  (let ((node (make-instance type
                             :title      title
                             :section    section
                             :out        children
                             :status     status
                             :priority   priority
                             :tags       tags
                             :properties properties)))
    (dolist (c (node.out node))
      (setf (node.in c) node))
    node))
