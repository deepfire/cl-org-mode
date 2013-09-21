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
;;;  Dressing
;;
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

(defun mapc-nodes-preorder (fn node &aux (seen (make-hash-table :test 'eq)))
  (labels ((rec (node)
             (unless (gethash node seen)
               (setf (gethash node seen) t)
               (dolist (child (node.out node))
                 (funcall fn node)
                 (rec child)))))
    (rec node)))

(defun mapc-leaf-nodes (fn node &aux (seen (make-hash-table :test 'eq)))
  (labels ((rec (node)
             (unless (gethash node seen)
               (setf (gethash node seen) t)
               (if-let ((child (node.out node)))
                       (dolist (c child)
                         (rec c))
                       (funcall fn node)))))
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

(defmacro do-leaf-nodes ((node graph) &body body)
  `(mapc-leaf-nodes (lambda (,node)
                  ,@body)
                ,graph))

(defmacro do-edges-preorder ((from to graph) &body body)
  `(mapc-edges-preorder (lambda (,from ,to)
                          ,@body)
                        ,graph))

(defmethod initialize-instance :after ((o org-node) &key out &allow-other-keys)
  (dolist (child out)
    (setf (node.in child) o)))

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

(defun call-with-raw-section-node-properties (section fn)
  (multiple-value-bind (drawer filtered-section)
      (if (cl-org-mode-raw:org-raw-property-drawer-p (second section))
          (values (second section) (cons (first section) (cddr section)))
          (values nil              section))
    (destructuring-bind (&key property-drawer contents) drawer
      (declare (ignore property-drawer))
      (funcall fn (mapcar (lambda (ast)
                            (destructuring-bind (&key property value) ast
                              (cons property value)))
                          contents)
               filtered-section))))

(defmacro with-raw-section-node-properties ((properties filtered-section) section &body body)
  `(call-with-raw-section-node-properties ,section (lambda (,properties ,filtered-section)
                                                     ,@body)))

(defun org-dress-property (ast)
  (destructuring-bind (&key property value) ast
    (make-instance 'org-keyword :name property :optional nil :value value)))

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
                   :static-properties node-properties)))))

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
                       :static-properties node-properties
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
