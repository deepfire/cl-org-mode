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

(defun pre-white (x)
  (mdo
    (many1? (choices #\Space #\Tab))
    x))

(defun pre-opt-white (x)
  (mdo
    (many? (choices #\Space #\Tab))
    x))

(defun post-white (x)
  (mdo
    (<- ret x)
    (result ret)))

(defun post-newline (x)
  (mdo (<- ret x)
       (newline)
       (result ret)))

(defun spacetab? ()
  (choice #\Space #\Tab))

(defun spacetabs? ()
  (many? (spacetab?)))

(defun spacetabs1? ()
  (many1? (spacetab?)))

(defun newline ()
  (choices #\Linefeed
           #\Newline
           #\Return
           (seq-list? #\Return #\Linefeed)))

(defun caseless (x)
  (choices (string-downcase x) (string-upcase x)))

;;;
;;; Tokens
(defun string-of (p)
  (hook? #'to-string (many? p)))

(defun string-of-1+ (p)
  (hook? #'to-string (many1? p)))

(defun plain-line ()
  (choices
   (mdo
     (<- first-char (line-constituent-but #\*))
     (<- second-char (if (char= first-char #\#)
                         (line-constituent-but #\+)
                         (line-constituent)))
     (<- rest       (string-of (line-constituent)))
     (result (concatenate 'string (list first-char second-char) rest)))
   (times? (line-constituent-but #\*) 1)
   ""))

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
    ":"
    (<- value (pre-opt-white (line-but-of)))
    (result (list (make-keyword (string-upcase name))
                  value))))

(defun org-simple-section ()
  (hook? (curry #'rejoin +newline-string+)
         (sepby? (plain-line) (newline))))

(defun org-header ()
  (mdo (<- mix (sepby? (choice
                        (org-option)
                        (org-simple-section))
                       (newline)))
       (result (append (list (list :header
                                   (apply #'append (remove-if-not #'consp mix))))
                       (when-let ((lines (remove-if #'consp mix)))
                         (list (list :section
                                     (rejoin +newline-string+ (remove-if #'consp mix)))))))))

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
    (<- parameters-and-pre-section (org-header))
    (<- entries    (many? (org-entry 1 (merge-parameters (first parameters-and-pre-section)
                                                         *org-default-parameters*))))
    (result (cons :org
                  (append parameters-and-pre-section entries)))))

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
      (<- section     (opt? (post-newline (org-section))))
      (<- children    (find-before? (org-entry (+ stars (if odd 2 1)) parameters)
                                    (choice
                                     (org-closing-headline-variants stars odd)
                                     (end?))))
      (result (append (list :entry headline)
                      (when section
                        (list section))
                      children)))))

;;;
;;; Section
(defun org-element-line ()
  (mdo
    (<- first-char (line-constituent-but #\*))
    (<- rest       (string-of (line-constituent)))
    (result (concatenate 'string (list first-char) rest))))

(defun org-element ()
  (hook? (curry #'rejoin +newline-string+)
         (sepby? (org-element-line) (newline))))

(defun org-section ()
  (mdo
    (<- content (sepby? (choices
                         (org-greater-element)
                         (org-element))
                        (newline)))
    (result (list :section content))))

;;;
;;; Headline
(defun org-title ()
  (hook? (curry #'rejoin " ")
         (sepby? (line-but-of-1+ #\: #\Space #\Tab)
                 (spacetabs1?))))

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
      (<- commentedp (opt? (pre-white (tag :commented (chook? t comment-keyword)))))
      (<- quotedp (opt? (pre-white (tag :quoted (chook? t quote-keyword)))))
      (<- keyword (opt? (pre-white (tag :todo (apply #'choices keywords)))))
      (<- priority (opt? (pre-white (tag :priority (org-priority priorities)))))
      (<- title (pre-white (tag :title (org-title))))
      (<- tags (opt? (pre-white (tag :tags (org-tags)))))
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
    (pre-white (caseless "#+BEGIN_"))
    (<- name (org-name))
    (<- parameters (opt? (pre-white (line-but-of))))
    (newline)
    (<- contents (find-before? (item)
                               (seq-list?
                                (caseless "#+END_") name (newline))))
    (pre-white (caseless "#+END_")) name (newline)
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
    (pre-white ":")
    (<- name (org-name))
    ":" (newline)
    (<- contents (find-before? (item)
                               (seq-list?
                                (pre-white ":END:") (newline))))
    (pre-white ":END:") (newline)
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
    (pre-white (caseless "#+BEGIN:"))
    (<- name (pre-white (line-but-of #\Space)))
    (<- parameters (opt? (pre-white (line-but-of))))
    (newline)
    (<- contents (find-before? (item)
                               (seq-list?
                                (pre-white "#+END:") (newline))))
    (pre-white "#+END:") (newline)
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
           (choices "CAPTION" "HEADER" "NAME" "PLOT" "RESULTS")))
    (mdo
      (pre-white "#+")
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
       (mdo "ATTR_"
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
