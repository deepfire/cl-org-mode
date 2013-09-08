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

(defun org-dress-node (ast)
  (assert (cl-org-mode-raw:org-raw-entry-p ast))
  (destructuring-bind (headline section &rest children) (rest ast)
    (assert (cl-org-mode-raw:org-raw-stars-p headline))
    (destructuring-bind (depth &key title commented quoted todo priority tags)
        (rest headline)
      (declare (ignore depth commented quoted))
      (let ((properties (when (and section
                                   (cl-org-mode-raw:org-raw-property-drawer-p (first section)))
                          (fourth (first section)))))
        (make-node title section (mapcar #'org-dress-node children)
                   :status     todo
                   :priority   priority
                   :tags       tags
                   :properties properties)))))

(defun org-dress (ast)
  (assert (cl-org-mode-raw:org-raw-p ast))
  (destructuring-bind (header section &rest children) (rest ast)
    (assert (cl-org-mode-raw:org-raw-header-p header))
    (destructuring-bind (&key title &allow-other-keys) (rest header)
      (make-node title section (mapcar #'org-dress-node children)
                 :type 'org-node
                 :properties (rest header)))))

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
