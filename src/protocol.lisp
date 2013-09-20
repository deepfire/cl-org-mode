(in-package :cl-org-mode)

(defclass node ()
  ((in  :initarg :in
        :type list
        :accessor node.in
        :initform nil
        :documentation "A list of immediate parent nodes. Warning: potentially a free-form graph here.")
   (out :initarg :out
        :type list
        :accessor node.out
        :initform nil
        :documentation "A list of immediate child nodes. Warning: potentially a free-form graph here."))
  (:documentation "Base class for all nodes"))

(defgeneric org-present (kind object stream))

(defgeneric properties-of (object))
(defgeneric dynamic-properties-of (object kind))
(defgeneric dynamic-property-p (cons))
