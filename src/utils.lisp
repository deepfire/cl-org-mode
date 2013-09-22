(in-package :cl-org-mode-utils)

;; set
(defun make-hashset (xs &key (test 'eql))
  (let ((set (make-hash-table :test test)))
    (dolist (x xs)
      (setf (gethash x set) set))
    set))

(defun copy-hashset (xs)
  (copy-hash-table xs))

(defun hashset-add (x xs)
  (setf (gethash x xs) x)
  xs)

(defun hashset-add-list (list xs)
  (dolist (x list)
    (hashset-add x xs)))

(defun hashset-remove (x xs)
  (remhash x xs)
  xs)

(defun hashset-pop (xs)
  (with-hash-table-iterator (pophash xs)
    (multiple-value-bind (present x) (pophash)
      (values x present))))

(defun in-hashset-p (x xs)
  (nth-value 1 (gethash x xs)))

(defun hashset-intersectionf (xs ys)
  (maphash (lambda (k v)
             (declare (ignore v))
             (unless (gethash k xs)
               (hashset-remove k ys)))
           ys)
  ys)

(defun hashset-unionf (xs ys)
  (maphash (lambda (k v)
             (declare (ignore v))
             (hashset-add k ys))
           xs)
  ys)

(defun hashset-intersection (xs ys)
  (let ((ret (copy-hash-table ys)))
    (hashset-intersectionf xs ret)
    ret))

(defun hashset-union (xs ys)
  (let ((ret (copy-hash-table ys)))
    (hashset-unionf xs ret)
    ret))

(defun hashset-list (xs)
  (let (ret)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k ret))
             xs)
    ret))

;; from pergamum
(defun slot-value* (object slot-name &optional (default :unbound-slot))
  "Return the value of slot named SLOT-NAME in OBJECT, when it is bound;
otherwise return DEFAULT, which defaults to :UNBOUND-SLOT."
  (cond ((not (slot-exists-p object slot-name))
         (format nil "#<MISSING-SLOT-~S>" slot-name))
        ((slot-boundp object slot-name)
         (slot-value object slot-name))
        (t default)))

;; from pergamum
(defmacro define-print-object-method (((object object-type &key (unreadable t) (type t) identity (unbound-slot-value :unbound-slot)) &rest slots) format-control
                                      &rest format-arguments)
  "Define a PRINT-OBJECT method for objects with OBJECT-TYPE.
The definition is a call to FORMAT, with FORMAT-CONTROL and 
FORMAT-ARGUMENTS, with SLOTS bound to slots of OBJECT, defaulting
when the respective slots are unbound, to :UNBOUND-SLOT.
When UNREADABLE is non-NIL, the call to FORMAT is wrapped into
PRINT-UNREADABLE-OBJECT, with TYPE and IDENTITY passed to it."
  (with-gensyms (stream)
    `(defmethod print-object ((,object ,object-type) ,stream)
       (symbol-macrolet ,(iter (for slot in slots)
                               (collect `(,slot (slot-value* ,object ',slot ,unbound-slot-value))))
         ,(if unreadable
              `(print-unreadable-object (,object ,stream :type ,type :identity ,identity)
                 (format ,stream ,format-control ,@format-arguments))
              `(format ,stream ,format-control ,@format-arguments))))))

;; from pergamum
(defun report-simple-condition (condition stream)
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

;; from pergamum
(defmacro define-simple-condition (base-type &key object-initarg (simple-condition-type 'simple-error) (signaler 'error))
  (let ((type (format-symbol t "SIMPLE-~A" base-type)))
    `(progn
       (define-condition ,type (,base-type ,simple-condition-type)
         ()
         (:report report-simple-condition))
       (defun ,base-type (,@(when object-initarg `(o)) format-control &rest format-arguments)
         (,signaler ',type ,@(when object-initarg `(,object-initarg o)) :format-control format-control :format-arguments format-arguments)))))

(defmacro define-simple-error (base-type &key object-initarg)
  "Define a simple error subclassing from BASE-TYPE and a corresponding
function, analogous to ERROR, but also optionally taking the object 
against which to err, and passing it to ERROR via the OBJECT-INITARG
keyword. The name of the simple error is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  `(define-simple-condition ,base-type :object-initarg ,object-initarg :simple-condition-type simple-error :signaler error))

(defmacro define-simple-warning (base-type &key object-initarg)
  "Define a simple warning subclassing from BASE-TYPE and a corresponding
function, analogous to WARN, but also optionally taking the object 
against which to warn, and passing it to WARN via the OBJECT-INITARG
keyword. The name of the simple warning is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  `(define-simple-condition ,base-type :object-initarg ,object-initarg :simple-condition-type simple-warning :signaler warn))
