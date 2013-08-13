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

;;
;; Extensions
(defmacro mdo* (&body spec)
  (with-gensyms (ret)
    `(named-seq*
      ,@(butlast spec)
      (<- ,ret ,(lastcar spec))
      ,ret)))

(defmacro with-next-char? ((var) &body body)
  (with-gensyms (inp unreadp)
    `(lambda (,inp)
       (let ((,unreadp t))
         (lambda ()
           (let ((,var (parser-combinators::context-peek ,inp)))
             ,@body)
           (when ,unreadp
             (setf ,unreadp nil)
             (make-instance 'parser-possibility :tree ,inp :suffix ,inp)))))))

(defun before* (p q)
  "Non-backtracking parser: Find a p before q, doesn't consume q."
  (parser-combinators::with-parsers (p q)
    (define-oneshot-result inp is-unread
      (let* ((p-result (funcall (funcall p inp)))
             (p-suffix (suffix-of p-result))
             (q-result (funcall (funcall q p-suffix))))
        (when (and p-result q-result)
          (make-instance 'parser-possibility :tree (tree-of p-result) :suffix p-suffix))))))

(defun chookahead? (result p)
  "Parser: return result if p matches, but do no advance"
  (parser-combinators::with-parsers (p)
    (define-oneshot-result inp is-unread
      (let ((p-result (funcall (funcall p inp))))
        (when p-result
          (make-instance 'parser-possibility :tree result :suffix inp))))))

(defun failing? (p)
  (named-seq*
   p
   (zero)))

(defun format? (fmt &rest args)
  (hook? (lambda (x) (declare (ignore x)) (apply #'format t fmt args)) (context?)))

(defmacro ? (x)
  (with-gensyms (res)
    `(named-seq*
      (format? "~S ?~%" ',x)
      (<- ,res ,x)
      (progn
        (format t "~S ok - ~S~%" ',x ,res)
        ,res))))

(defun next-char-in? (name)
  (with-next-char? (char)
    (format t "~A, next char: ~S~%" name char)))


(defmacro c? (x)
  (with-gensyms (res char)
    `(named-seq*
      (with-next-char? (,char)
        (format t "~S ? (next ~S)~%" ',x ,char))
      (<- ,res ,x)
      (progn
        (format t "~S ok - ~S~%" ',x ,res)
        ,res))))

;;;
;;; Tools
(defun unzip (fn sequence &key (key #'identity))
  (iter (for elt in sequence)
        (if (funcall fn (funcall key elt))
            (collect elt into yes)
            (collect elt into no))
        (finally (return (values yes no)))))

(defun tree-getf (tree &rest keys)
  (if keys
      (apply #'access (getf tree (first keys)) (rest keys))
      tree))

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

(defun string-of (p)
  (hook? #'to-string (many? p)))

(defun string-of-1+ (p)
  (hook? #'to-string (many1? p)))

(defun line-but-of (&rest except)
  (string-of (apply #'line-constituent-but except)))

(defun line-but-of-1+ (&rest except)
  (string-of-1+ (apply #'line-constituent-but except)))

(defun spacetab? ()
  (choice #\Space #\Tab))

(defun spacetabs? ()
  (many? (spacetab?)))

(defun spacetabs1? ()
  (many1? (spacetab?)))

(defun pre-white1? (x)
  (mdo*
    (spacetabs1?)
    x))

(defun pre-white? (x)
  (mdo*
    (spacetabs?)
    x))

(defun pre-newline? (x &key retain-newline)
  (named-seq*
   (newline)
   (<- ret x)
   (if retain-newline
       (concatenate 'string +newline-string+ ret)
       ret)))

(defun newline ()
  (chook? +newline-string+
          (choices #\Newline
                   #\Linefeed
                   #\Return
                   (seq-list? #\Return #\Linefeed))))

(defun caseless (x)
  (choices (string-downcase x) (string-upcase x)))

(defun post-newline? (x)
  (named-seq*
    (<- ret x)
    (newline)
    ret))

(defun opt-and-pre-newline? (x)
  (opt?
   (mdo*
    (newline)
    x)))

(defun find-sepby-before? (p sep stop)
  (choices
   (chook? nil stop)
   (named-seq*
     (<- head (except? p stop))
     (<- tail (find-before* (mdo* sep (except? p stop))
                            stop))
     (cons head tail))))

(defun find-sepby1-before? (p sep stop)
  (named-seq*
    (<- head (except? p stop))
    (<- tail (find-before* (mdo* sep (except? p stop))
                           (mdo* sep stop)))
    (cons head tail)))

(defun find-sepby1-before-nonsep? (p sep stop)
  (mdo
    (<- head p)
    (hook? (curry #'cons head)
           (choices
            (chookahead? nil stop)
            (find-before* (mdo* sep p)
                          stop)))))

(defun upto-end-of-line? (x)
  (mdo (<- xs (find-before? x (choice (newline)
                                      (end?))))
       (if (not (endp (rest xs)))
           (zero)
           (result (first xs)))))

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
(defun org-name ()
  (string-of-1+ (choices (alphanum?) #\_ #\-)))

(defun org-tag-name ()
  (string-of-1+ (choices (alphanum?) #\_ #\@ #\# #\%)))

(defparameter *org-startup*
  '((:overview :content :showall :showeverything)
    (:indent :noindent)
    (:align :noalign)
    (:inlineimages :noinlineimages)
    (:latexpreview :nolatexpreview)
    (:logdone :lognotedone :nologdone)
    (:logrepeat :lognoterepeat :nologrepeat)
    (:logreschedule :lognotereschedule :nologreschedule)
    (:logredeadline :lognoteredeadline :nologredeadline)
    (:logrefile :lognoterefile :nologrefile)
    (:lognoteclock-out :nolognoteclock-out)
    (:logdrawer :nologdrawer)
    (:logstatesreversed :nologstatesreversed)
    (:hidestars :showstars)
    (:indent :noindent)
    (:odd :oddeven)
    (:customtime)
    (:constcgs :constsi)
    (:fninline :fnnoinline :fnlocal)
    (:fnprompt :fnauto :fnconfirm :fnplain)
    (:fnadjust :nofnadjust)
    (:hideblocks :nohideblocks)
    (:entitiespretty :entitiesplain)
    ))

(let ((set (make-hash-table :test 'eq)))
  (dolist (xs *org-startup*)
    (dolist (x xs)
      (setf (gethash x set) xs)))
  (defun org-startup-option? (x)
    (gethash x set)))

(defun org-header ()
  "15.6 Summary of in-buffer settings"
  (flet ((org-keywords-as-plist (xs)
           (iter (for x in xs)
                 (appending (destructuring-bind (&key keyword value) x
                              (list (make-keyword (string-upcase keyword))
                                    value)))))
         (keywords-as-flags (xs)
           (mapcan (rcurry #'list t) xs))
         (parse-startup (xs)
           (let ((all-opts (mapcar (compose #'make-keyword #'string-upcase)
                                   (split-sequence:split-sequence #\Space xs
                                                                  :remove-empty-subseqs t)))
                 decided-co-sets
                 valid unknown duplicate conflicted)
             (dolist (o all-opts)
               (let* ((co-set (org-startup-option? o))
                      (conflicted? (find co-set decided-co-sets))
                      (conflict (find-if (lambda (x) (find x valid)) co-set)))
                 (cond ((not co-set)
                        (push o unknown))
                       ((find o valid)
                        (push o duplicate))
                       (conflicted?
                        (push o conflicted)
                        (push conflict conflicted))
                       (t
                        (push co-set decided-co-sets)
                        (push o valid)))))
             (setf valid (set-difference valid conflicted))
             (values all-opts valid unknown duplicate conflicted))))
    (named-seq*
     (<- mix (opt? (org-section)))
     (multiple-value-bind (raw-keywords section-content)
         (unzip (lambda (x) (and (consp x) (eq :keyword (car x)))) (second mix))
       (let ((keyword-plist (org-keywords-as-plist raw-keywords)))
         (format t ";;; header options:~{ ~S~}~%" keyword-plist)
         (when section-content
           (format t ";;; header section: ~S~%" section-content))
         (destructuring-bind (&key (startup "") &allow-other-keys) keyword-plist
           (multiple-value-bind (all valid unknown duplicate conflicted)
               (parse-startup startup)
             (format t ";;; header startup:~%")
             (when valid
               (format t ";;;    valid:     ~{ ~S~}~%" valid))
             (when unknown
               (format t ";;;    unknown:   ~{ ~S~}~%" unknown))
             (when duplicate
               (format t ";;;    duplicate: ~{ ~S~}~%" duplicate))
             (when conflicted
               (format t ";;;    conflicted:~{ ~S~}~%" conflicted))
             `((:header
                ,(append (remove-from-plist keyword-plist :startup)
                         (when valid
                           (list :startup (keywords-as-flags valid)))
                         (when (or unknown conflicted)
                           (list :startup-all (keywords-as-flags all)))))
               ,@(when section-content
                       (list (list :section section-content)))))))))))

(defun header-nothing-p (x)
  (and (endp (cdr x))
       (endp (cadar x))))

(defparameter *org-default-startup*
  '(:odd              nil
    :comment-keyword  "COMMENT"
    :quote-keyword    "QUOTE"
    :footnote-title   "Footnotes"
    :keywords        ("TODO" "DONE")
    :priorities      ("A" "B" "C"))
  "So called 'startup' options, in absence of any headers.")

(defun merge-startup (stronger weaker)
  (append stronger weaker))

;;;
;;; Whole thing
(defun org-parser ()
  (mdo
    (<- initial (org-header))
    (if (header-nothing-p initial)
        (context?)
        (newline))
    (<- entries    (sepby? (org-top-entry (tree-getf (first initial) :header :startup))
                           (newline)))
    (opt? (newline))
    (result (cons :org
                  (append initial entries)))))

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
  (defun string-position-context (cache offset &key (around 0))
    (let* ((succ (cl-containers:find-successor-node cache (cons (1+ offset) nil)))
           (pred (if succ
                     (cl-containers:predecessor cache succ)
                     (cl-containers::last-node cache))))
      (labels ((wind-rec (node n fn)
                 (if (plusp n)
                     (or (when-let ((prev (funcall fn cache node)))
                           (when (cl-containers:element prev)
                             ;; ACHTUNG!! this is a bug in CL-CONTAINERS!
                             (wind-rec prev (1- n) fn)))
                         node)
                     node))
               (wind (node n fn)
                 (let ((ret (wind-rec node n fn)))
                   (assert ret)
                   (unless (cl-containers:element ret)
                       (format t "WIND: ~S ~S ~S~%"
                               ret (cl-containers:element ret) (cl-containers:collect-items cache)))
                   (cl-containers:element ret))))
        (destructuring-bind (lposn . lineno) (cl-containers:element pred)
          ;; (format t "? ~D -> p ~S s ~S ~D@~D~%" offset pred succ lineno lposn)
          (values lineno lposn
                  (car (wind pred around      #'cl-containers:predecessor))
                  (1-
                   (car (wind pred (1+ around) #'cl-containers:successor))))))))
  (defun string-position-context-full (string cache posn &key (around 0))
    (multiple-value-bind (lineno lposn ctxstart ctxend) (string-position-context
                                         cache posn :around around)
      (let ((lend (or (position #\Newline string :start lposn)
                      (length string))))
        (values lineno lposn lend
                (subseq string lposn lend)
                (subseq string ctxstart ctxend)
                (- posn lposn)))))
  (defun show-string-position (place string posn cache &key (around 0))
    (multiple-value-bind (lineno lposn rposn line ctx col) (string-position-context-full string cache posn :around around)
      (declare (ignore lposn rposn line))
      (let ((fmt (format nil "; at ~~A, line ~~D, col ~~D:~~%~~A~~%~~~D@T^~~%" col)))
        (format t fmt place (1+ lineno) (1+ col) ctx))))
  (defun try-org-file (filename &key profile
                       &aux
                         (string (alexandria:read-file-into-string filename))
                         (cache  (make-string-position-cache string)))
    (flet ((string-context (posn &key (around 0))
             (string-position-context-full string cache posn :around around))
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
        (declare (ignore vector-context))
        (if (and successp (null front))
            (let ((top-hits (top-hits seen-positions 25)))
              (when (eq profile :full)
                (iter (for line in (split-sequence:split-sequence #\Newline string))
                      (for lineno from 0)
                      (format t ";   ~A~%" line)
                      (dolist (line-top (remove lineno top-hits :test #'/= :key #'third))
                        (destructuring-bind (posn hits lineno lineposn ctxstart ctxend) line-top
                          (declare (ignore ctxstart ctxend))
                          (let* ((col (- posn lineposn))
                                 (fmt (format nil ";;; ~~~D@T^  ~~D hits, posn ~~D:~~D:~~D~~%" col)))
                            (format t fmt hits posn lineno col))))))
              (when profile
                (format t ";;; total context references: ~D~%"
                        (apply #'+ (hash-table-values seen-positions))))
              result)
            (let ((failure-posn (slot-value (slot-value front 'parser-combinators::context) 'position)))
              (multiple-value-bind (lineno lposn rposn line ctx col) (string-context failure-posn :around 2)
                (declare (ignore lposn rposn line))
                (format t "; ~A at line ~D, col ~D:~%~A~%"
                        (if successp "partial match" "failure") (1+ lineno) (1+ col) ctx))))
        #+nil
        (iter (for (posn lineno hits) in )
              (print-hit posn hits)))))
  (defun test (&key profile)
    (dolist (f *org-files*)
      (time (try-org-file f :profile profile)))))

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
(defun org-top-entry (startup)
  (org-entry 1 (merge-startup startup *org-default-startup*)))

(defun org-child-entry (stars startup &aux (odd (getf startup :odd)))
  (org-entry (+ stars (if odd 2 1)) startup))

(defun org-entry (stars &optional (startup *org-default-startup*))
  (mdo
    (<- headline (org-headline stars))
    (<- body
        (opt?
         (mdo
           (<- section  (opt-and-pre-newline?
                         (org-section)))
           (<- children (opt-and-pre-newline?
                         (find-sepby1-before-nonsep?
                          (org-child-entry stars startup)
                          (newline)
                          (choices
                           (seq-list*
                            (newline)
                            (choice (end?)
                                    (org-closing-headline-variants stars startup)))
                           (end?)))))
           (result (progn
                     (append (when section
                               (list section))
                             children))))))
    (result
     (append (list :entry headline)
             body))))

;;;
;;; Section
(defun org-section ()
  (choice1
   (chook? (list :section "") (end?))
   (mdo
     (<- content (find-sepby1-before-nonsep?
                  (choices
                   (org-greater-element)
                   (org-affiliated-keyword)
                   (org-element))
                  (newline)
                  (choice (seq-list? (newline) "*")
                          (end?))))
     (if-let ((filtered-content (remove "" content :test #'equal)))
       (result (list :section filtered-content))
       (zero)))))

(defun org-greater-signature ()
  (pre-white? (choices "#+" (bracket? ":" (org-name) ":"))))

(defun org-element ()
  "Actually org-paragraph."
  (choice
   (named-seq*
     (<- lines (find-sepby1-before-nonsep?
                (org-element-line)
                (newline)
                (choices
                 ;; (org-greater-element)
                 (seq-list? (newline) (choice1 "*"
                                               (org-greater-signature)))
                 (end?))))
     (rejoin +newline-string+ lines))
   (chook? "" (end?))))

(defun org-element-line ()
  (choices
   (chookahead? "" (end?))
   (chookahead? "" (newline))
   (except? (mdo
              (<- first-char (line-constituent-but #\*))
              (<- rest       (find-before* (line-constituent-but)
                                           (choice1 (newline)
                                                    (end?))))
              (result (concatenate 'string (list first-char) rest)))
            (org-greater-signature))))

(defparameter *testcases*
  '(;; 0
    ("* a" (:ENTRY (:TITLE "a")))
    ;; 1
    ("* a
"          (:ENTRY (:TITLE "a") (:SECTION "")))
    ;; 2
    ("* a
   a text" (:ENTRY (:TITLE "a")
            (:SECTION ("   a text"))))
    ;; 3
    ("* a
** b
"          (:ENTRY (:TITLE "a")
            (:ENTRY (:TITLE "b") (:SECTION ""))))
    ;; 4
    ("* a
   a text
** b
"          (:ENTRY (:TITLE "a")
            (:SECTION ("   a text"))
            (:ENTRY (:TITLE "b") (:SECTION ""))))
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

(defun test-org-entry (&optional trace)
  (values-list
   (mapcan (lambda (tc n)
             (when trace
               (format t "---------------- #~D~%" n))
             (destructuring-bind (input expected) tc
               (let ((output (parse-string* (org-entry 1) input)))
                 (unless (equal output expected)
                   (when trace
                     (format t "-~%~S~%~S~%~S~%" n input output))
                   (list '_ n input output)))))
           *testcases*
           (iota (length *testcases*)))))

;;;
;;; Headline
(defun org-title ()
  (hook? #'to-string
         (find-before* (line-constituent-but)
                       (seq-list? (opt? (pre-white1? (org-tags)))
                                  (choice1 (newline) (end?))))))

(defun org-priority (priorities)
  (named-seq* "[#"
              (<- priority (apply #'choices priorities))
              "]"
              (string priority)))

(defun org-tags ()
  (named-seq* ":"
              (<- tags (sepby? (org-tag-name) #\:))
              ":"
              tags))

(defun org-stars (n)
  (times? #\* n))

(defun org-closing-headline-variants (current startup)
  (let ((variants (loop :for x :downfrom current :to 1 :by (if (getf startup :odd) 2 1)
                     :collect x)))
    (apply #'choices (mapcar (lambda (n)
                               (seq-list? (org-stars n)
                                          " "))
                             variants))))

(defun org-headline (nstars &optional (startup *org-default-startup*))
  (destructuring-bind (&key comment-keyword quote-keyword keywords priorities
                       &allow-other-keys) startup
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
(defun org-drawer ()
  "Deviation: does not parse own contents."
  (mdo
    (<- name (pre-white? (bracket? ":" (org-name) ":")))
    (newline)
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
  (mdo
    (pre-white? "#+")
    (choices
     (named-seq* (<- key      (org-name))
                 (<- optional (opt? (named-seq*
                                      "["
                                      (<- ret (line-but-of-1+ #\[ #\]))
                                      "]"
                                      ret)))
                 ": "
                 (<- value    (line-but-of))
                 (append (list :keyword key)
                         (when optional
                           (list :optional optional))
                         (list :value value)))
     (named-seq* (caseless "ATTR_")
                 (<- backend (org-name))
                 ": "
                 (<- value   (line-but-of))
                 (list :attribute backend
                       :value value)))))

;; (org-parse 
;; "#+STARTUP: hidestars odd

;; * a

;;   a text

;; ** b
;;    b text

;; ")
