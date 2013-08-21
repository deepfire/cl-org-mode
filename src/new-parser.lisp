(in-package :cl-org-mode)

;;;
;;; Tools
(defun unzip (fn sequence &key (key #'identity))
  (iter (for elt in sequence)
        (if (funcall fn (funcall key elt))
            (collect elt into yes)
            (collect elt into no))
        (finally (return (values yes no)))))

(define-constant +newline-string+ (coerce #(#\Newline) 'string) :test #'equal)

(defun strconcat (xs)
  (apply #'concatenate 'string xs))

;;;
;;; Primitives
(defun char-bag (bag)
  (sat (lambda (x) (find x bag :test #'char=))))

(defun char-not-bag (bag)
  (sat (lambda (x) (not (find x bag :test #'char=)))))

(defun line-constituent-but (except)
  (char-not-bag (list* #\Linefeed #\Newline #\Return except)))

(defun line-constituent-but* (&rest except)
  (line-constituent-but except))

(defun string-of (p)
  (between* p 0 nil 'string))

(defun string-of-1+ (p)
  (between* p 1 nil 'string))

(defun line-but-of (&rest except)
  (string-of (line-constituent-but except)))

(defun line-but-of-1+ (&rest except)
  (string-of-1+ (line-constituent-but except)))

(defun caseless (x)
  (choices1 (string-downcase x) (string-upcase x)))

(defun spacetab ()
  (char-bag '(#\Space #\Tab)))

(defun newline ()
  (chook? +newline-string+
          (choices1 #\Newline
                    (seq-list* #\Return #\Newline)
                    #\Return)))

(defun eol ()
  (choice1 (newline)
           (end?)))

(defun line-without-eol ()
  (named-seq*
   (<- string (line-but-of))
   (eol)
   string))

(defun line-full ()
  (hook? (lambda (x) (concatenate 'string x +newline-string+))
         (line-without-eol)))

(defun spacetabs ()
  (between* (spacetab) nil nil 'string))

(defun spacetabs1 ()
  (between* (spacetab) 1 nil 'string))

(defun pre-white1? (x)
  (mdo*
    (spacetabs1)
    x))

(defun pre-white? (x)
  (mdo*
    (spacetabs)
    x))

(defun org-name ()
  (string-of-1+ (choices1 (alphanum?) #\_ #\-)))

(defun tag (tag x)
  (hook? (lambda (x)
           (list tag x))
         x))
;;;;
;;;; The grammar encoded therein roughly approximates http://orgmode.org/worg/dev/org-syntax.html
;;;;
;;;
;;;  Element   http://orgmode.org/worg/dev/org-syntax.html#Elements
(defun org-greater-signature ()
  (choice1 "#+" (bracket? ":" (org-name) ":")))

(defun org-block-end-signature ()
  (seq-list* (caseless "#+END")
             (choice1 "_"
                      (before* (opt* ":")
                               (eol)))))

(defun org-drawer-end-signature ()
  (caseless ":END:"))

(defun org-boundary (&key for)
  (choices1 (if (member for '(:section-element :entry-section))
                "*" (zero))
            (pre-white?
             (ecase for
               (:section-element   (org-greater-signature))
               (:element-in-block  "#+")
               (:element-in-drawer (bracket? ":" (org-name) ":"))
               (:entry-section     (choice1 (org-block-end-signature)
                                            (org-drawer-end-signature)))
               (:section-in-block  (org-block-end-signature))
               (:section-in-drawer (org-drawer-end-signature))))
            (end?)))

(defun org-element-line (&key in)
  (choice1
   (except? (named-seq*
             (<- first-char (line-constituent-but (when (eq in :section-element)
                                                    '(#\*))))
             (<- line       (line-full))
             (concatenate 'string (list first-char) line))
            (ecase in
              (:section-element   (org-greater-signature))
              (:element-in-block  "#+")
              (:element-in-drawer (bracket? ":" (org-name) ":"))))
   (eol)))

(defun org-element (&key (kind :section-element))
  "Actually org-paragraph."
  (mdo
    (<- lines (find-before* (org-element-line :in kind)
                            (org-boundary :for kind)))
    (if lines
        (result (strconcat lines))
        (zero))))

;;;
;;;  Affiliated keyword   http://orgmode.org/worg/dev/org-syntax.html#Affiliated_keywords
(defun org-affiliated-keyword ()
  "Deviation: allows optionals for keys other than CAPTION and RESULTS."
  (named-seq* "#+"
              (<- name      (except? (org-name) (seq-list* (caseless "END")
                                                           (choices1 " " "_" ":"))))
              (<- optional (opt* (bracket? "[" (line-but-of-1+ #\[ #\]) "]")))
              (opt* ":") (opt* " ")
              (<- value    (line-without-eol))
              (let ((attributep (starts-with-subseq "ATTR_" name)))
                (append (if attributep
                            (list :attribute (subseq name 5))
                            (list :keyword name))
                        (when optional
                          (list :optional optional))
                        (list :value value)))))

;;;
;;;  Section   http://orgmode.org/worg/dev/org-syntax.html#Headlines_and_Sections
(defun org-section (&key (kind :entry-section))
  (named-seq*
   (<- content (find-before*
                (choices1
                 (org-element :kind (ecase kind
                                      (:entry-section     :section-element)
                                      (:section-in-block  :element-in-block)
                                      (:section-in-drawer :element-in-drawer)))
                 (case kind
                   (:section-in-drawer (org-greater-nondrawer-element))
                   (t                  (org-greater-element)))
                 (org-affiliated-keyword))
                (org-boundary :for kind)))
   (when-let ((filtered-content (remove "" content :test #'equal)))
     (list :section filtered-content))))

;;;
;;;  Greater element   http://orgmode.org/worg/dev/org-syntax.html#Greater_Elements
;;
;; part of the section->greater-element->section loop
(defun org-greater-nondrawer-element ()
  (choice1
   (org-greater-block)
   (org-dynamic-block)))

(defun org-greater-element ()
  (choice1
   (org-greater-nondrawer-element)
   (org-drawer)))

;;;
;;;  Greater blocks   http://orgmode.org/worg/dev/org-syntax.html#Greater_Blocks
(defun org-greater-block ()
  "Deviation: does not parse own contents."
  (mdo
    (pre-white? (caseless "#+BEGIN_"))
    (<- name (org-name))
    (<- parameters (opt* (pre-white1? (line-without-eol))))
    (<- contents (delayed? (org-section :kind :section-in-block)))
    (pre-white? (caseless "#+END_")) (caseless name) (opt* (seq-list* (spacetabs1) (line-but-of))) (eol)
    (result (list :block name
                  :parameters parameters
                  :contents contents))))

;;;
;;;  Drawer   http://orgmode.org/worg/dev/org-syntax.html#Drawers_and_Property_Drawers
(defun org-property ()
  (named-seq*
   (spacetabs)
   (<- name (bracket? ":" (except? (org-name) "END") ":"))
   (<- value (opt* (pre-white1? (line-but-of))))
   (spacetabs) (eol)
   (list :property name :value value)))

(defun org-drawer ()
  "Deviation: does not parse own contents."
  (mdo
    (<- name    (pre-white? (bracket? ":" (except? (org-name) (caseless "END")) ":")))
    (opt* (line-but-of)) (eol)
    (<- contents (cond ((string-equal name "PROPERTIES")
                        (many* (org-property)))
                       (t
                        (delayed? (org-section :kind :section-in-drawer)))))
    (choices1
     (seq-list* (pre-white? (caseless ":END:")) (spacetabs) (eol))
     (chookahead? t (choices1 "*"
                              (seq-list* (spacetabs) "#+")
                              (end?))))
    (result (list :drawer name
                  :contents contents))))

;;;
;;;  Dynamic block   http://orgmode.org/worg/dev/org-syntax.html#Dynamic_Blocks
(defun org-dynamic-block ()
  (mdo
    (pre-white? (caseless "#+BEGIN:"))
    (<- name (pre-white1? (line-but-of #\Space)))
    (<- parameters (opt* (pre-white? (line-without-eol))))
    (<- contents (delayed? (org-section :kind :section-in-block)))
    (pre-white? (seq-list* (caseless "#+END")
                           (before* (opt* ":")
                                    (seq-list* (opt* (seq-list* (spacetabs1)
                                                                (line-but-of)))
                                               (eol)))))
    (result (list :dynamic-block name
                  :parameters parameters
                  :contents contents))))

;;;
;;;  Headline   http://orgmode.org/worg/dev/org-syntax.html#Headlines_and_Sections
(defun org-priority (priorities)
  (named-seq* "[#"
              (<- priority (apply #'choices1 priorities))
              "]"
              (string priority)))

(defun org-tag-name ()
  (string-of-1+ (choices1 (alphanum?) #\_ #\@ #\# #\%)))

(defun org-tags ()
  (named-seq* ":"
              (<- tags (sepby* (org-tag-name) #\:))
              ":"
              tags))

(defun org-title ()
  (find-before* (line-constituent-but nil)
                (seq-list* (opt* (pre-white1? (org-tags)))
                           (eol))
                'string))

(let ((star-cache (make-hash-table :test 'eq)))
  (defun org-stars (n)
    (or (gethash n star-cache)
        (setf (gethash n star-cache)
              (seq-list* (between* #\* n nil 'string)
                         (chookahead? t (choice1 " " (eol))))))))

(defparameter *org-default-startup*
  '(:odd              nil
    :comment-keyword  "COMMENT"
    :quote-keyword    "QUOTE"
    :footnote-title   "Footnotes"
    :keywords        ("TODO" "DONE")
    :priorities      ("A" "B" "C"))
  "So called 'startup' options, in absence of any headers.")

(defun org-headline (nstars &optional (startup *org-default-startup*))
  (destructuring-bind (&key comment-keyword quote-keyword keywords priorities
                       &allow-other-keys) startup
    (named-seq*
     (<- stars (org-stars nstars))
     (<- commentedp (opt* (pre-white1? (tag :commented (chook? t (before* comment-keyword
                                                                          (spacetabs1)))))))
     (<- quotedp (opt* (pre-white1? (tag :quoted (chook? t (before* quote-keyword
                                                                    (spacetabs1)))))))
     (<- keyword (opt* (pre-white1? (tag :todo (before* (apply #'choices1 keywords)
                                                        (spacetabs1))))))
     (<- priority (opt* (pre-white1? (tag :priority (before* (org-priority priorities)
                                                             (spacetabs1))))))
     (<- title (tag :title (choice1 (pre-white1? (org-title)) "")))
     (<- tags (opt* (pre-white1? (tag :tags (org-tags)))))
     (spacetabs) (eol)
     (list* :stars (length (first stars))
            (append commentedp quotedp keyword priority title tags)))))

;;;
;;;  Entry   http://orgmode.org/worg/dev/org-syntax.html#Headlines_and_Sections
(defun org-entry (stars)
  (mdo
    (<- headline (org-headline stars))
    (<- body     (named-seq*
                  (<- section  (delayed? (org-section :kind :entry-section)))
                  (<- children (let ((de-facto-stars (getf headline :stars)))
                                 (many* (delayed? (org-entry (+ de-facto-stars 1))))))
                  (append (when section
                            (list section))
                          children)))
    (result (append (list :entry headline)
                    body))))

;;;
;;;  Header   http://orgmode.org/manual/In_002dbuffer-settings.html#In_002dbuffer-settings
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

(defun org-header (&key trace)
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
     (<- mix (opt* (org-section :kind :entry-section)))
     (multiple-value-bind (raw-keywords section-content)
         (unzip (lambda (x) (and (consp x) (eq :keyword (car x)))) (second mix))
       (let ((keyword-plist (org-keywords-as-plist raw-keywords)))
         (when trace
           (format t ";;; header options:~{ ~S~}~%" keyword-plist)
           (when section-content
             (format t ";;; header section: ~S~%" section-content)))
         (destructuring-bind (&key (startup "") &allow-other-keys) keyword-plist
           (multiple-value-bind (all valid unknown duplicate conflicted)
               (parse-startup startup)
             (when trace
               (format t ";;; header startup:~%")
               (when valid
                 (format t ";;;    valid:     ~{ ~S~}~%" valid))
               (when unknown
                 (format t ";;;    unknown:   ~{ ~S~}~%" unknown))
               (when duplicate
                 (format t ";;;    duplicate: ~{ ~S~}~%" duplicate))
               (when conflicted
                 (format t ";;;    conflicted:~{ ~S~}~%" conflicted)))
             `((:header
                ,(append (remove-from-plist keyword-plist :startup)
                         (when valid
                           (list :startup (keywords-as-flags valid)))
                         (when (or unknown conflicted)
                           (list :startup-all (keywords-as-flags all)))))
               ,@(when section-content
                       (list (list :section section-content)))))))))))

;;;
;;; Whole thing
(defun org-parser ()
  (mdo
    (<- initial (org-header))
    (<- entries    (many* (org-entry 1)))
    (result (cons :org
                  (append initial entries)))))

(defun org-parse-string (string)
  (parse-string* (org-parser) string))

(defun org-parse (x)
  (etypecase x
    (string
     (org-parse-string x))
    ((or pathname stream)
     (org-parse-string (read-file-into-string x)))))
