(in-package :cl-org-mode)

;;;
;;; Node classes
;;;
(defclass org-document (org-node)
  ((options    :reader org-document-options :initarg :options)))

(defclass org-node (node)
  ((title      :reader title-of      :initarg :title)
   (section    :reader section-of    :initarg :section)
   (status     :reader status-of     :initarg :status)
   (priority   :reader priority-of   :initarg :priority)
   (tags       :reader tags-of       :initarg :tags)
   (static-properties :reader static-properties-of :initarg :static-properties)))

(define-print-object-method ((o org-node) title out)
    "~S ~:[leaf~;~:*~D children~]" title (when (plusp (length out))
                                       (length out)))

(defmethod properties-of ((o org-node))
  (static-properties-of o))

(defmethod dynamic-property-p ((o cons))
  nil)

(defmethod initialize-instance :after ((o org-node) &key out &allow-other-keys)
  (dolist (child out)
    (setf (node.in child) (list o))))

(defun make-node (title section children &key status priority tags static-properties
                                           (type 'org-node))
  (make-instance type
                 :title      title
                 :section    section
                 :out        children
                 :status     status
                 :priority   priority
                 :tags       tags
                 :static-properties static-properties))

;;;
;;; Node content object classes
;;;
(defclass org-container ()
  ((children   :reader children-of   :initarg :children :type 'list)))

(defclass org-named-container (org-container)
  ((name       :reader name-of       :initarg :name)))

(defclass org-section    (org-container) ())
(defclass org-properties (org-container) ())
(defclass org-drawer     (org-named-container) ())
(defclass org-block      (org-named-container)
  ((parameters :reader parameters-of :initarg :parameters)))

(defclass org-keyword ()
  ((name       :reader name-of       :initarg :name)
   (optional   :reader optional-of   :initarg :optional)
   (value      :reader value-of      :initarg :value)))

;;;
;;; Convenience package for happy WITH-SLOTS usage..
;;;
(defpackage :cl-org-mode-slots
  (:import-from
   #:cl-org-mode
   ;; node
   #:in #:out
   ;; org-node
   #:title #:section #:status #:priority #:tags #:static-properties
   ;; document
   #:options
   ;; elements
   #:children
   #:parameters
   #:name #:value #:optional)
  (:export
   ;; node
   #:in #:out
   ;; org-node
   #:title #:section #:status #:priority #:tags #:static-properties
   ;; document
   #:options
   ;; elements
   #:children
   #:parameters
   #:name #:value #:optional))
