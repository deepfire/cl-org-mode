(in-package :cl-org-mode)


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
           (make-instance 'org-drawer :name name :children (children-of (org-dress-section contents))))
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
