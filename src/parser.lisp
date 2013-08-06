(in-package :cl-org-mode)

(defmethod read-next-node (node (next-node null) stream) 
  "This method is called when we don't yet know what the next node is"
  (let (stack)
    (loop for char = (read-char stream nil)
       :if (null char) 
       :do (return (finalize-node node NIL stack))
       :else 
       :do (push char stack)
	 (multiple-value-bind (new-node old-stack)
		 (find-next-node node next-node stack)	   
	       (when new-node 
		 (return (finalize-node node new-node old-stack)))))))

(defmethod read-next-node (node (next-node node) stream)
  "When we know what the node is already, just return it"
  next-node)

(defmethod read-next-node :around (node next-node stream)
  (let ((*dispatchers* (node-dispatchers node)))
  ;(warn "DISPATHERS FOR ~A ~A: ~A" node next-node *dispatchers*)
    (call-next-method)))


(defmethod find-next-node (node next-node stack)
  (loop for object in *dispatchers*
       :do (multiple-value-bind (result old-stack)
	       (node-start object stack)
	     (when result (return (values result old-stack))))))

(declaim (optimize debug safety))

;;;
;;; Tools

(defun to-string (xs)
  (coerce xs 'string))

(defun to-symbol (package xs)
  (intern (string-upcase (to-string xs)) package))

(defun tag (tag x)
  (hook? (lambda (x)
           (list tag x))
         x))

(defun intersperse (elt xs)
  (nbutlast
   (loop :for x :in xs
      :collect x
      :collect elt)))

(define-constant +newline-string+ (coerce #(#\Newline) 'string) :test #'equal)

(defun rejoin (x xs)
  (apply #'concatenate 'string (intersperse x xs)))

(defun rejoined-lines (x)
  (hook? (curry #'rejoin +newline-string+)
         (sepby? x (newline))))

;;;
;;; Primitives
(defun char-bag (bag)
  (sat (lambda (x) (member x bag :test #'char=))))

(defun char-not-bag (bag)
  (sat (lambda (x) (not (member x bag :test #'char=)))))

(defun line-constituent-but (&rest except)
  (char-not-bag (list* #\Linefeed #\Newline #\Return except)))

(defun line-of (&rest constituents)
  (string-of (char-bag constituents)))

(defun line-but-of (&rest except)
  (string-of (apply #'line-constituent-but except)))

(defun line-but-of-1+ (&rest except)
  (string-of-1+ (apply #'line-constituent-but except)))

(defun line-constituent ()
  (line-constituent-but))

(defun name-constituent ()
  (choices (alphanum?) #\_ #\-))

(defun tag-constituent ()
  (choices (alphanum?) #\_ #\@ #\# #\%))

(defun spacetab? ()
  (choice #\Space #\Tab))

(defun spacetabs? ()
  (many? (spacetab?)))

(defun spacetabs1? ()
  (many1? (spacetab?)))

(defun pre-white1? (x)
  (mdo
    (spacetabs1?)
    x))

(defun pre-white? (x)
  (mdo
    (spacetabs?)
    x))

(defun post-white? (x)
  (mdo
    (<- ret x)
    (result ret)))

(defun pre-newline? (x &key retain-newline)
  (mdo (newline)
       (<- ret x)
       (result (if retain-newline
                   (concatenate 'string +newline-string+ ret)
                   ret))))

(defun newline ()
  (chook? +newline-string+
          (choices #\Linefeed
                   #\Newline
                   #\Return
                   (seq-list? #\Return #\Linefeed))))

(defun caseless (x)
  (choices (string-downcase x) (string-upcase x)))

(defun post-newline? (x)
  (mdo
    (<- ret x)
    (newline)
    (result ret)))

(defun opt-and-pre-newline? (x)
  (opt?
   (mdo
     (newline)
     x)))

(defun find-sepby-before? (p sep stop)
  (choices
   (chook? nil stop)
   (mdo
     (<- head (except? p stop))
     (<- tail (find-before* (mdo sep
                                 (except? p stop))
                            stop))
     (result (cons head tail)))))

(defun find-sepby1-before? (p sep stop)
  (mdo
    (<- head (except? p stop))
    (<- tail (find-before* (mdo sep
                                (except? p stop))
                           (mdo sep stop)))
    (result (cons head tail))))

(defun upto-newline? (x)
  (mdo (<- xs (find-before? x (newline)))
       (if (not (endp (rest xs)))
           (zero)
           (result (first xs)))))

(defun format? (fmt &rest args)
  (hook? (lambda (x) (declare (ignore x)) (apply #'format t fmt args)) (context?)))

(defun failing? (p)
  (mdo
    p
    (zero)))

(defmacro ? (x)
  (with-gensyms (res)
    `(mdo (format? "~S ?~%" ',x)
          (<- ,res ,x)
          (format? "~S ok - ~S~%" ',x ,res)
          (result ,res))))

;;;
;;; Tokens
(defun string-of (p)
  (hook? #'to-string (many? p)))

(defun string-of-1+ (p)
  (hook? #'to-string (many1? p)))

(defun org-name ()
  (string-of (name-constituent)))

;;;
;;; Org Syntax (draft)
;;
;; ..A core concept in this syntax is that only headlines and sections are context-free.
;; Every other syntactical part only exists within specific environments.
;;
;; Three categories are used to classify these environments: “Greater elements”, “elements”,
;; and “objects”, from the broadest scope to the narrowest. The word “element” is used for both
;; Greater and non-Greater elements, the context should make that clear.
;;
;; The paragraph is the unit of measurement. An element defines syntactical parts that are at
;; the same level as a paragraph, i.e. which cannot contain or be included in a paragraph. An
;; object is a part that could be included in an element. Greater elements are all parts that
;; can contain an element.
;;
;; Empty lines belong to the largest element ending before them. For example, in a list, empty
;; lines between items belong are part of the item before them, but empty lines at the end of a
;; list belong to the plain list element.
;;
;; Unless specified otherwise, case is not significant.
;;
;;
;;;
;;; Header
(defun option-value-constituent ()
  (char-not-bag '(#\Linefeed #\Newline #\Return #\Space #\Tab)))

(defun org-option ()
  (mdo
    "#+"
    (<- name (org-name))
    (if (let ((up (string-upcase name)))
          (or (starts-with-subseq "BEGIN_" up)
              (string= "BEGIN" up)))
        (zero)
        (context?))
    ":"
    (<- value (pre-white? (line-but-of)))
    (result (list (make-keyword (string-upcase name))
                  value))))

(defun org-header ()
  (mdo (<- mix (sepby? (choice
                        (org-option)
                        (org-section))
                       (newline)))
       (result (append (list (list :header
                                   (apply #'append (remove :section mix :key #'car))))
                       (when-let ((sections (remove :section mix :key #'car :test (complement #'eql))))
                         (list (cons :section
                                     (apply #'append (mapcar #'rest sections)))))))))

(defun header-nothing-p (x)
  (and (null (cadr x))
       (endp (cddr x))))

(defparameter *org-default-parameters*
  '(:odd              nil
    :comment-keyword  "COMMENT"
    :quote-keyword    "QUOTE"
    :footnote-title   "Footnotes"
    :keywords        ("TODO" "DONE")
    :priorities      ("A" "B" "C"))
  "Parameters, in absence of any headers.")

(defun merge-parameters (stronger weaker)
  (append stronger weaker))

;;;
;;; Whole thing
(defun org-parser ()
  (mdo
    (<- header-and-pre-section (org-header))
    (unless (header-nothing-p header-and-pre-section)
      (newline))
    (<- entries    (sepby? (org-entry 1 (merge-parameters (first header-and-pre-section)
                                                          *org-default-parameters*))
                           (newline)))
    (result (cons :org
                  (append header-and-pre-section entries)))))

;; (progn (require :cl-org-mode) (in-package :cl-org-mode))
#-nil
(progn
  (defun make-string-position-cache (string)
    (let ((cache (make-instance 'cl-containers:red-black-tree :key #'car)))
      (iter (with line = 0)
            (for posn from 0)
            (for char in-string string)
            (for pchar previous char initially #\Newline)
            (when (char= pchar #\Newline)
              ;; (format t "> posn ~D/~D line ~D~%" posn (length string) line)
              (cl-containers:insert-item cache (cons posn line))
              (incf line)))
      cache))
  (defun string-position-context (cache offset)
    (let* ((succ (cl-containers:find-successor-node cache (cons (1+ offset) nil)))
           (pred (if succ
                     (cl-containers:predecessor cache succ)
                     (cl-containers::last-node cache))))
      (destructuring-bind (lposn . lineno) (cl-containers:element pred)
        ;; (format t "? ~D -> p ~S s ~S ~D@~D~%" offset pred succ lineno lposn)
        (values lineno lposn))))
  (defun string-position-context-full (string cache posn)
    (multiple-value-bind (lineno lposn) (string-position-context cache posn)
      (let ((lend (or (position #\Newline string :start lposn)
                      (length string))))
        (values lineno lposn lend (subseq string lposn lend) (- posn lposn)))))
  (defun show-string-position (place string posn &optional (cache (make-string-position-cache string)))
    (multiple-value-bind (lineno lposn rposn line col) (string-position-context-full string cache posn)
      (declare (ignore lposn rposn))
      (let ((fmt (format nil "; at ~~A, line ~~D, col ~~D:~~%; ~~A~~%; ~~~D@T^~~%" col)))
        (format t fmt place (1+ lineno) (1+ col) line))))
  (defun try-org-file (filename &aux
                                  (string (alexandria:read-file-into-string filename))
                                  (cache  (make-string-position-cache string)))
    (flet ((string-context (posn)
             (string-position-context-full string cache posn))
           (top-hits (hash n &aux
                           (alist (hash-table-alist hash))
                           (sorted (sort alist #'> :key #'cdr))
                           (top (subseq sorted 0 (min n (hash-table-count hash)))))
             (mapcar (lambda (x)
                       (destructuring-bind (posn . hits) x
                         (list* posn hits (multiple-value-list (string-position-context cache posn)))))
                     top))
           #+nil
           (show-fn (posn)
             (show-string-position "x" string posn cache))
           (print-hit (posn hits)
             (show-string-position (format nil "~D hits" hits)
                                   string posn cache)))
      (declare (ignorable #'print-hit))
      (format t ";;; trying: ~S~%" filename)
      (multiple-value-bind (result vector-context successp front seen-positions) (org-parse string)
        (if (and successp (null front))
            (let ((top-hits (top-hits seen-positions 25)))
              (iter (for line in (split-sequence:split-sequence #\Newline string))
                    (for lineno from 0)
                    (format t ";   ~A~%" line)
                    (dolist (line-top (remove lineno top-hits :test #'/= :key #'third))
                      (destructuring-bind (posn hits lineno lineposn) line-top
                        ;; (declare (ignore lineno))
                        (let* ((col (- posn lineposn))
                               (fmt (format nil ";;; ~~~D@T^  ~~D hits, posn ~~D:~~D:~~D~~%" col)))
                          (format t fmt hits posn lineno col))))
                    (finally
                     (format t ";;; total context references: ~D~%"
                             (apply #'+ (hash-table-values seen-positions)))))
              result)
            (let ((failure-posn (slot-value (slot-value front 'parser-combinators::context) 'position)))
              (multiple-value-bind (lineno lposn rposn line col) (string-context failure-posn)
                (declare (ignore lposn rposn))
                (format t "; ~A at line ~D, col ~D:~%~A~%"
                        (if successp "partial match" "failure") (1+ lineno) (1+ col) line))))
        #+nil
        (iter (for (posn lineno hits) in )
              (print-hit posn hits)))))
  (defun test ()
    (dolist (f *org-files*)
      (try-org-file f))))

(defun org-parse-string (string)
  (parse-string* (org-parser) string))

(defun org-parse (x)
  (etypecase x
    (string
     (org-parse-string x))
    ((or pathname stream)
     (org-parse-string (read-file-into-string x)))))

;;;
;;; Headlines and Sections
;;
;; A headline is defined as:
;;
;; STARS KEYWORD PRIORITY TITLE TAGS
;;
;; STARS is a string starting at column 0, containing at least one asterisk (and up to
;; org-inlinetask-min-level if org-inlinetask library is loaded) and ended by a space
;; character. The number of asterisks is used to define the level of the headline. It’s the
;; sole compulsory part of a headline.
;;
;; KEYWORD is a TODO keyword, which has to belong to the list defined in
;; org-todo-keywords-1. Case is significant.
;;
;; PRIORITY is a priority cookie, i.e. a single letter preceded by a hash sign # and enclosed
;; within square brackets.
;;
;; TITLE can be made of any character but a new line. Though, it will match after every other
;; part have been matched.
;;
;; TAGS is made of words containing any alpha-numeric character, underscore, at sign, hash sign
;; or percent sign, and separated with colons.
;; 
;; If the first word appearing in the title is org-comment-string, the headline will be
;; considered as “commented”. If that first word is org-quote-string, it will be considered as
;; “quoted”. In both situations, case is significant.
;; 
;; If its title is org-footnote-section, it will be considered as a “footnote section”. Case is
;; significant.
;;
;; If org-archive-tag is one of its tags, it will be considered as “archived”. Case is
;; significant.
;;
;; A headline contains directly one section (optionally), followed by any number of deeper
;; level headlines.
;;
;; A section contains directly any greater element or element. Only a headline can contain a
;; section. As an exception, text before the first headline in the document also belongs to a
;; section.
;;
;; If a quoted headline contains a section, the latter will be considered as a “quote section”.
;;
;;;
;;; Entry
(defun org-entry (stars &optional (parameters *org-default-parameters*))
  (destructuring-bind (&key odd &allow-other-keys) parameters
    (mdo
      (<- headline    (org-headline stars))
      (<- body
          (opt?
           (mdo
             (<- section  (opt-and-pre-newline?
                           (org-section)))
             (<- children (opt-and-pre-newline?
                           (find-sepby-before? (org-entry (+ stars (if odd 2 1)) parameters)
                                               (newline)
                                               (choice
                                                (org-closing-headline-variants stars odd)
                                                (end?)))))
             
             (result (append (when section
                               (list section))
                             children)))))
      (result (append (list :entry headline)
                      body)))))

;;;
;;; Section
(defun org-section ()
  (mdo
    (<- content (sepby1? (choices
                          (org-greater-element)
                          (org-element))
                         (newline)))
    (result (list :section content))))

(defun org-element ()
  "Actually org-paragraph."
  (mdo
    (<- lines (find-sepby1-before?
               (org-element-line)
               (newline)
               (choices
                ;; (org-greater-element)
                "*"
                (pre-white? (choices "#+" (org-drawer-name)))
                (end?))))
       (result (rejoin +newline-string+ lines))))

(defun org-element-line ()
  (choice1
   (mdo
     (<- first-char (line-constituent-but #\*))
     (<- rest       (find-before* (line-constituent) (newline)))
     (result (concatenate 'string (list first-char) rest)))
   (upto-newline? "")))

(defparameter *testcases*
  '(;; 0
    ("* a" (:ENTRY (:TITLE "a")))
    ;; 1
    ("* a
"          (:ENTRY (:TITLE "a")))
    ;; 2
    ("* a
   a text" (:ENTRY (:TITLE "a")
            (:SECTION ("   a text"))))
    ;; 3
    ("* a
** b
"          (:ENTRY (:TITLE "a")
            (:ENTRY (:TITLE "b"))))
    ;; 4
    ("* a
   a text
** b
"          (:ENTRY (:TITLE "a")
            (:SECTION ("   a text"))
            (:ENTRY (:TITLE "b"))))
    ;; 5
    ("* a

  a text

** b
   b text

"          (:ENTRY (:TITLE "a")
            (:SECTION ("
  a text
"))
            (:ENTRY (:TITLE "b")
                    (:SECTION
                     ("   b text

")))))))

(defun test-org-entry ()
  (values-list
   (mapcan (lambda (tc n)
             (destructuring-bind (input expected) tc
               (let ((output (parse-string* (org-entry 1) input)))
                 (unless (equal output expected)
                   (list '_ n input output)))))
           *testcases*
           (iota (length *testcases*)))))

;;;
;;; Headline
(defun org-title ()
  (hook? #'to-string
         (find-before* (line-constituent)
                       (seq-list? (opt? (pre-white? (org-tags)))
                                  (newline)))))

(defun org-priority (priorities)
  (mdo "[#"
       (<- priority (apply #'choices priorities))
       "]"
       (result (string priority))))

(defun org-tags ()
  (mdo ":"
       (<- tags (sepby? (string-of (tag-constituent)) #\:))
       ":"
       (result tags)))

(defun org-stars (n)
  (times? #\* n))

(defun org-closing-headline-variants (current odd)
  (let ((variants (loop :for x :downfrom current :to 1 :by (if odd 2 1)
                     :collect x)))
    (apply #'choices (mapcar (lambda (n)
                               (seq-list? (org-stars n)
                                          " "))
                             variants))))

(defun org-headline (nstars &optional (parameters *org-default-parameters*))
  (destructuring-bind (&key comment-keyword quote-keyword keywords priorities
                       &allow-other-keys) parameters
    (mdo
      (org-stars nstars)
      (<- commentedp (opt? (pre-white1? (tag :commented (chook? t comment-keyword)))))
      (<- quotedp (opt? (pre-white1? (tag :quoted (chook? t quote-keyword)))))
      (<- keyword (opt? (pre-white1? (tag :todo (apply #'choices keywords)))))
      (<- priority (opt? (pre-white1? (tag :priority (org-priority priorities)))))
      (<- title (pre-white1? (tag :title (org-title))))
      (<- tags (opt? (pre-white1? (tag :tags (org-tags)))))
      (spacetabs?)
      (result (append commentedp quotedp keyword priority title tags)))))

;;;
;;; Greater element
(defun org-greater-element ()
  (choices
   (org-greater-block)
   (org-drawer)
   (org-dynamic-block)))

;;;
;;; Greater blocks
;;
;; Greater blocks consist in the following pattern:
;;
;; #+BEGIN_NAME PARAMETERS
;; CONTENTS
;; #+END_NAME
;;
;; NAME can contain any non-whitespace character.
;;
;; PARAMETERS can contain any character other than new line, and can be omitted.
;;
;; If NAME is “CENTER”, it will be a “center block”. If it is “QUOTE”, it will be a “quote
;; block”.
;;
;; If the block is neither a center block, a quote block or a block element, it will be a
;; “special block”.
;;
;; CONTENTS can contain any element, except : a line #+END_NAME on its own. Also lines
;; beginning with STARS must be quoted by a comma.
;;
(defun org-greater-block ()
  "Deviation: does not parse own contents."
  (mdo
    (pre-white? (caseless "#+BEGIN_"))
    (<- name (org-name))
    (<- parameters (opt? (pre-white1? (line-but-of))))
    (newline)
    (<- contents (find-before* (item)
                               (seq-list?
                                (caseless "#+END_") name (newline))))
    (pre-white? (caseless "#+END_")) name
    (result (list :block name
                  :parameters parameters
                  :contents (to-string contents)))))

;;;
;;; Drawer
;;
;; Pattern for drawers is:
;;
;; :NAME:
;; CONTENTS
;; :END:
;;
;; NAME has to either be “PROPERTIES” or belong to org-drawers list.
;;
;; If NAME is “PROPERTIES”, the drawer will become a “property drawer”.
;;
;; In a property drawers, CONTENTS can only contain node property elements. Otherwise it can
;; contain any element but another drawer or property drawer.
;;
(defun org-drawer-name ()
  (mdo
    ":"
    (<- name (org-name))
    ":"
    (result name)))

(defun org-drawer ()
  "Deviation: does not parse own contents."
  (mdo
    (<- name (pre-white? (org-drawer-name))) (newline)
    (<- contents (find-before* (item)
                               (seq-list?
                                (pre-white? (caseless ":END:")) (newline))))
    (pre-white? (caseless ":END:"))
    (result (list :drawer name
                  :contents (to-string contents)))))

;;;
;;; Dynamic block
;;
;; Pattern for dynamic blocks is:
;;
;; #+BEGIN: NAME PARAMETERS
;; CONTENTS
;; #+END:
;;
;; NAME cannot contain any whitespace character.
;;
;; PARAMETERS can contain any character and can be omitted.
;;
(defun org-dynamic-block ()
  (mdo
    (pre-white? (caseless "#+BEGIN:"))
    (<- name (pre-white? (line-but-of #\Space)))
    (<- parameters (opt? (pre-white? (line-but-of))))
    (newline)
    (<- contents (find-before* (item)
                               (seq-list?
                                (pre-white? (caseless "#+END:")) (newline))))
    (pre-white? (caseless "#+END:"))
    (result (list :dynamic-block name
                  :parameters parameters
                  :contents (to-string contents)))))

;;;
;;; Affiliated keyword
;;
;; With the exception of inlinetasks, items, planning, clocks, node properties and table rows,
;; every other element type can be assigned attributes.
;;
;; This is done by adding specific keywords, named “affiliated keywords”, just above the
;; element considered, no blank line allowed.
;;
;; Affiliated keywords are built upon one of the following patterns: “#+KEY: VALUE”,
;; “#+KEY[OPTIONAL]: VALUE” or “#+ATTR_BACKEND: VALUE”.
;;
;; KEY is either “CAPTION”, “HEADER”, “NAME”, “PLOT” or “RESULTS” string.
;;
;; BACKEND is a string constituted of alpha-numeric characters, hyphens or underscores.
;;
;; OPTIONAL and VALUE can contain any character but a new line. Only “CAPTION” and “RESULTS”
;; keywords can have an optional value.
;;
;; An affiliated keyword can appear more than once if KEY is either “CAPTION” or “HEADER” or if
;; its pattern is “#+ATTR_BACKEND: VALUE”.
;;
;; “CAPTION”, “AUTHOR”, “DATE” and “TITLE” keywords can contain objects in their value and
;; their optional value, if applicable.
;;
(defun org-affiliated-keyword ()
  "Deviation: allows optionals for keys other than CAPTION and RESULTS."
  (flet ((keyword-key-name ()
           (choices (caseless "CAPTION") (caseless "HEADER") (caseless "NAME") (caseless "PLOT") (caseless "RESULTS"))))
    (mdo
      (pre-white? "#+")
      (choices
       (mdo (<- key      (keyword-key-name))
            (<- optional (opt? (mdo
                                 "[" 
                                 (<- ret (line-but-of-1+ #\[ #\]))
                                 "]"
                                 (result ret))))
            ": "
            (<- value    (line-but-of))
            (result (append (list :key key)
                            (when optional
                              (list :optional optional))
                            (list :value value))))
       (mdo (caseless "ATTR_")
            (<- backend (org-name))
            ": "
            (<- value   (line-but-of))
            (result (list :attr backend
                          :value value)))))))

;; (org-parse 
;; "#+STARTUP: hidestars odd

;; * a

;;   a text

;; ** b
;;    b text

;; ")
