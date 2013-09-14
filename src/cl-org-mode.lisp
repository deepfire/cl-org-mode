(in-package :cl-org-mode)

(define-condition org-condition ()                      ())
(define-condition org-error     (org-condition error)   ())
(define-condition org-warning   (org-condition warning) ())

(define-simple-error   org-error)
(define-simple-warning org-warning)

(define-condition org-parse-error (org-error)
  ((source   :reader source-of   :initarg :source)
   (line     :reader line-of     :initarg :line)
   (column   :reader column-of   :initarg :column)))

;;;;
;;;  Dressing and presentation
;;
(defmethod org-present ((kind (eql :flat)) (o string) s)
  (write-string o s))

(defclass org-document (org-node)
  ((options    :reader org-document-options :initarg :options)))

(defun org-present-flat-properties (properties stream)
  (write-line ":PROPERTIES:" stream)
  (dolist (p properties)
    (format stream ":~A:~:[~; ~:*~A~]~%" (name-of p) (value-of p)))
  (write-line ":END:" stream))

(defmethod org-present ((kind (eql :flat)) (o org-document) s)
  (with-slots (options title properties section) o
    (iter (for (option value . rest) on options by #'cddr)
          (format s "#+~A: ~A~%" (symbol-name option) value))
    (format s "#+TITLE: ~A~%" title)
    (when properties
      (org-present-flat-properties properties s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (org-present :flat c s))))

(defclass org-node (node)
  ((title      :reader title-of      :initarg :title)
   (section    :reader section-of    :initarg :section)
   (status     :reader status-of     :initarg :status)
   (priority   :reader priority-of   :initarg :priority)
   (tags       :reader tags-of       :initarg :tags)
   (properties :reader properties-of :initarg :properties)))

(define-print-object-method ((o org-node) title out)
    "~S~:[~; ~:*~D children~]" title (when (plusp (length out))
                                       (length out)))

(defun mapc-nodes-preorder (fn node &aux (seen (make-hash-table :test 'eq)))
  (labels ((rec (node)
             (unless (gethash node seen)
               (setf (gethash node seen) t)
               (dolist (child (node.out node))
                 (funcall fn node)
                 (rec child)))))
    (rec node)))

(defun mapc-edges-preorder (fn node &aux (seen (make-hash-table :test 'equal)))
  (labels ((rec (from to)
             (unless (gethash (cons from to) seen)
               (setf (gethash (cons from to) seen) t)
               (funcall fn from to)
               (dolist (to-to (node.out to))
                 (rec to to-to)))))
    (dolist (child (node.out node))
      (rec node child))))

(defmacro do-nodes-preorder ((node graph) &body body)
  `(mapc-nodes-preorder (lambda (,node)
                          ,@body)
                        ,graph))

(defmacro do-edges-preorder ((from to graph) &body body)
  `(mapc-edges-preorder (lambda (,from ,to)
                          ,@body)
                        ,graph))

(defmethod org-present ((kind (eql :flat)) (o org-node) s)
  (with-slots (status priority title tags properties section) o
    (format s "*~:[~; ~:*~A~]~:[~; ~:*~A~] ~A~:[~; ~:*~A~]~%"
            status priority title tags)
    (when properties
      (org-present-flat-properties properties s))
    (org-present :flat section s)
    (dolist (c (node.out o))
      (org-present :flat c s))))

(defmethod initialize-instance :after ((o org-node) &key out &allow-other-keys)
  (dolist (child out)
    (setf (node.in child) o)))

(defun make-node (title section children &key status priority tags properties
                                           (type 'org-node))
  (make-instance type
                 :title      title
                 :section    section
                 :out        children
                 :status     status
                 :priority   priority
                 :tags       tags
                 :properties properties))

(defclass org-container ()
  ((children   :reader children-of   :initarg :children :type 'list)))

(defclass org-named-container (org-container)
  ((name       :reader name-of       :initarg :name)))

(defclass org-section    (org-container) ())
(defclass org-properties (org-container) ())
(defclass org-drawer     (org-named-container) ())
(defclass org-block      (org-named-container)
  ((parameters :reader parameters-of :initarg :parameters)))

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

(defclass org-keyword ()
  ((name       :reader name-of       :initarg :name)
   (optional   :reader optional-of   :initarg :optional)
   (value      :reader value-of      :initarg :value)))

(defmethod org-present ((kind (eql :flat)) (o org-keyword) s)
  (format s "#+~A:~:[~;~:* [~A]~] ~A~%" (name-of o) (optional-of o) (value-of o)))

(defun org-dress-property (ast)
  (destructuring-bind (&key property value) ast
    (make-instance 'org-keyword :name property :optional nil :value value)))

(defun call-with-raw-section-node-properties (section fn)
  (multiple-value-bind (drawer filtered-section)
      (if (cl-org-mode-raw:org-raw-property-drawer-p (second section))
          (values (second section) (cons (first section) (cddr section)))
          (values nil              section))
    (destructuring-bind (&key property-drawer contents) drawer
      (declare (ignore property-drawer))
      (funcall fn (mapcar #'org-dress-property contents) filtered-section))))

(defmacro with-raw-section-node-properties ((properties filtered-section) section &body body)
  `(call-with-raw-section-node-properties ,section (lambda (,properties ,filtered-section)
                                                     ,@body)))

(defun org-dress-element (ast)
  (if (stringp ast)
      ast
      (destructuring-bind (kind name &key parameters contents optional value) ast
        (case kind
          ((:block :dynamic-block)
           (make-instance 'org-block        :name name :children (children-of (org-dress-section contents)) :parameters parameters))
          (:basic-drawer
           (make-instance 'org-basic-drawer :name name :children (children-of (org-dress-section contents))))
          (:property-drawer
           (make-instance 'org-properties              :children (mapcar #'org-dress-property contents)))
          ((:keyword :attribute)
           (make-instance 'org-keyword      :name name :optional optional :value value))
          (t
           (org-error "~@<Unexpected AST in place of element: ~S.~:@>" (first ast)))))))

(defun org-dress-section (ast)
  (make-instance 'org-section :children (mapcar #'org-dress-element (rest ast))))

(defun org-dress-node (ast)
  (assert (cl-org-mode-raw:org-raw-entry-p ast))
  (destructuring-bind (headline section &rest children) (rest ast)
    (assert (cl-org-mode-raw:org-raw-stars-p headline))
    (assert (or (null section)
                (cl-org-mode-raw:org-raw-section-p section)))
    (destructuring-bind (depth &key title commented quoted todo priority tags) (rest headline)
      (declare (ignore depth commented quoted))
      (with-raw-section-node-properties (node-properties section) section
        (make-node title (when section
                           (org-dress-section section))
                   (mapcar #'org-dress-node children)
                   :status     todo
                   :priority   priority
                   :tags       tags
                   :properties node-properties)))))

(defun org-dress (ast)
  (assert (cl-org-mode-raw:org-raw-p ast))
  (destructuring-bind (header section &rest children) (rest ast)
    (assert (cl-org-mode-raw:org-raw-header-p header))
    (assert (or (null section)
                (cl-org-mode-raw:org-raw-section-p section)))
    (destructuring-bind (&key title &allow-other-keys) (rest header)
      (with-raw-section-node-properties (node-properties section) section
        (make-instance 'org-document
                       :title title
                       :section (when section
                                  (org-dress-section section))
                       :out (mapcar #'org-dress-node children)
                       :properties node-properties
                       :options (remove-from-plist (rest header) :title :startup-all))))))

(defun org-parse (org)
  (let ((text (etypecase org
                (string org)
                (pathname (read-file-into-string org)))))
    (parser-combinators-debug:with-position-cache (cache text)
      (multiple-value-bind (result vector-context successp front seen-positions) (cl-org-mode-raw:org-raw-parse org)
        (declare (ignore vector-context seen-positions))
        (if successp
            (org-dress result)
            (let ((failure-posn (parser-combinators:position-of front)))
              (multiple-value-bind (line column) (parser-combinators-debug:string-position failure-posn)
                (error 'org-parse-error :source org :line line :column column))))))))

;;;
;;; Convenience package for happy WITH-SLOTS usage..
;;;
(defpackage :cl-org-mode-slots
  (:import-from
   #:cl-org-mode
   ;; node
   #:in #:out
   ;; org-node
   #:title #:section #:status #:priority #:tags #:properties
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
   #:title #:section #:status #:priority #:tags #:properties
   ;; document
   #:options
   ;; elements
   #:children
   #:parameters
   #:name #:value #:optional))
