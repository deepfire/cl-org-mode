(in-package :cl-org-mode)

(defparameter *org-entry-testcases*
  '( ;; 0
    ("* a" (:ENTRY (:STARS 1 :TITLE "a")))
    ;; 1
    ("* a
"          (:ENTRY (:STARS 1 :TITLE "a")))
    ;; 2
    ("* a
   a text" (:ENTRY (:STARS 1 :TITLE "a")
            (:SECTION ("   a text
"))))
    ;; 3
    ("* a
** b
"          (:ENTRY (:STARS 1 :TITLE "a")
            (:ENTRY (:STARS 2 :TITLE "b"))))
    ;; 4
    ("* a
   a text
** b
"          (:ENTRY (:STARS 1 :TITLE "a")
            (:SECTION ("   a text
"))
            (:ENTRY (:STARS 2 :TITLE "b"))))
    ;; 5
    ("* a

  a text

** b
   b text

"          (:ENTRY (:STARS 1 :TITLE "a")
            (:SECTION ("
  a text

"))
            (:ENTRY (:STARS 2 :TITLE "b")
                    (:SECTION
                     ("   b text

")))))
    ("* a

** b
"          (:ENTRY (:STARS 1 :TITLE "a")
            (:SECTION ("
"))
            (:ENTRY (:STARS 2 :TITLE "b"))))))

(defun test-org-entry (&optional (trace t) (debug t)
                       &aux (*debug-mode* debug))
  (values-list
   (mapcan (lambda (tc n)
             (when trace
               (format t "---------------- #~D~%" n))
             (destructuring-bind (input expected) tc
               (with-position-cache (cache input)
                 (let ((output (parse-string* (org-entry 1) input)))
                   (unless (equal output expected)
                     (when trace
                       (format t "-~%~S~%~S~%~S~%" n input output))
                     (list '_ n input output))))))
           *org-entry-testcases*
           (iota (length *org-entry-testcases*)))))

;;;
;;; Testing
(defun string-position-context-full (string cache posn &key (around 0))
  (multiple-value-bind (lineno lposn ctxstart ctxend)
      (parser-combinators-debug::string-position-context cache posn :around around)
    (let ((lend (or (position #\Newline string :start lposn)
                    (length string))))
      (values lineno (- posn lposn) lposn lend
              (subseq string lposn lend)
              (subseq string ctxstart ctxend)))))

(defun show-string-position (place string posn cache &key (around 0))
  (multiple-value-bind (lineno col lposn rposn line ctx) (string-position-context-full string cache posn :around around)
    (declare (ignore lposn rposn line))
    (let ((fmt (format nil "; at ~~A, line ~~D, col ~~D:~~%~~A~~%~~~D@T^~~%" col)))
      (format t fmt place (1+ lineno) (1+ col) ctx))))

(defun org-complexity (text &optional (parser (if (and (plusp (length text))
                                                       (starts-with #\* text))
                                                  (curry #'org-entry 1)
                                                  #'org-element)))
  (with-position-cache (cache text)
    (multiple-value-bind (result vector-context successp front seen-positions)
        (parse-string* (funcall parser) text)
      (declare (ignorable result vector-context front))
      #+nil
      (format t "parser ~S, text:~%~S~%result ~S, vector-context ~S, successp ~S, front ~S, seen-positions ~S~%"
              parser text result vector-context successp front seen-positions)
      (unless successp
        (error "Failed to parse (using ~S):~%~A"
               parser (format nil "--- 8< ---~%~S~%--- >8 ---~%" text)))
      (apply #'+ (hash-table-values seen-positions)))))

(defparameter *overhead-measured-parsers*
  `(("element-line" nil nil ,#'org-element-line)
    ("element"      nil nil ,#'org-element)
    ("section"      nil t   ,#'org-section)
    ("entry"        t   t   ,(curry #'org-entry 1))
    ("org"          t   t   ,#'org-parser)))

(defun parser-context-overheads (&key
                                   debug
                                   (depth-range 5) (depth-start 0)
                                   (text-length-range 15)
                                   (parsers *overhead-measured-parsers*)
                                   (text-meat-char #\x)
                                   trailing-newline-p
                                 &aux
                                   (*debug-mode* debug))
  (labels ((strconcat* (&rest xs)
             (strconcat xs))
           (generate-overhead-org (depth text-length)
             (iter (for i below (1+ depth))
                   (when (plusp i)
                     (collecting (strconcat* (make-string i :initial-element #\*)
                                             " "
                                             (string (code-char (+ i (1- (char-code #\a)))))
                                             +newline-string+)
                                 into lines))
                   (finally
                    (return (strconcat
                             (append lines
                                     (list (when (plusp text-length)
                                             (strconcat*
                                              (make-string (1- text-length)
                                                           :initial-element text-meat-char)
                                              "x")))
                                     (when trailing-newline-p
                                       (list +newline-string+)))))))))
    (values-list (map-product (lambda (parser-spec depth)
                                (destructuring-bind (name entry-capable-p empty-capable-p parser)
                                    parser-spec
                                  (when (if (zerop depth)
                                            (not entry-capable-p)
                                            entry-capable-p)
                                    (let* ((seq (iter (for text-length below text-length-range)
                                                      (collect
                                                          (when (or (plusp text-length)
                                                                    empty-capable-p)
                                                            (org-complexity
                                                             (generate-overhead-org depth text-length)
                                                             parser)))))
                                           (tail (last seq 2))
                                           (rate (- (second tail) (first tail))))
                                      (append (list name) seq
                                              (list (list :char-cost rate)))))))
                              parsers (iota depth-range :start depth-start)))))

(defun try-org-file (filename &key profile debug (parser #'org-parser)
                     &aux
                       (*debug-mode* debug)
                       (string (alexandria:read-file-into-string filename)))
  (with-position-cache (cache string)
    (flet ((string-context (posn &key (around 0))
             (string-position-context-full string cache posn :around around))
           (top-hits (hash n &aux
                           (alist (hash-table-alist hash))
                           (sorted (sort alist #'> :key #'cdr))
                           (top (subseq sorted 0 (min n (hash-table-count hash)))))
             (mapcar (lambda (x)
                       (destructuring-bind (posn . hits) x
                         (list* posn hits (multiple-value-list (parser-combinators-debug::string-position-context cache posn)))))
                     top))
           (print-hit (posn hits)
             (show-string-position (format nil "~D hits" hits)
                                   string posn cache)))
      (declare (ignorable #'print-hit))
      (format t ";;;~%;;;~%;;; trying: ~S~%" filename)
      (multiple-value-bind (result vector-context successp front seen-positions)
          (parse-string* (funcall parser) string)
        (declare (ignore vector-context))
        (if (and successp (null front))
            (let ((top-hits (top-hits seen-positions 25))
                  (total-references (reduce #'+ (hash-table-values seen-positions) :initial-value 0)))
              (when (eq profile :full)
                (Iter (for line in (split-sequence:split-sequence #\Newline string))
                      (for lineno from 0)
                      (format t ";   ~A~%" line)
                      (dolist (line-top (remove lineno top-hits :test #'/= :key #'third))
                        (destructuring-bind (posn hits lineno lineposn ctxstart ctxend) line-top
                          (declare (ignore ctxstart ctxend))
                          (let* ((col (- posn lineposn))
                                 (fmt (format nil ";;; ~~~D@T^  ~~D hits, posn ~~D:~~D:~~D~~%" col)))
                            (format t fmt hits posn lineno col))))))
              (when profile
                (format t ";;; total context references: ~D~%" total-references))
              (values result total-references))
            (let ((failure-posn (slot-value (slot-value front 'parser-combinators::context) 'position)))
              (multiple-value-bind (lineno col lposn rposn line ctx) (string-context failure-posn :around 2)
                (declare (ignore lposn rposn line))
                (format t "; ~A at line ~D, col ~D:~%~A~%"
                        (if successp "partial match" "failure") (1+ lineno) col ctx))))
        #+nil
        (iter (for (posn lineno hits) in )
              (print-hit posn hits))))))

(defun maybe-time (timep f)
  (if timep (time (funcall f)) (funcall f)))

(defmacro with-maybe-time ((time) &body body)
  `(maybe-time ,time (lambda () ,@body)))

(defun test (org-files &key profile debug
             &aux
               (total-contexts 0)
               (succeeded 0))
  (with-maybe-time (profile)
    (dolist (f org-files)
      (with-maybe-time (profile)
        (multiple-value-bind (success ncontexts) (try-org-file f :profile profile :debug debug)
          (when ncontexts
            (incf total-contexts ncontexts))
          (when success
            (incf succeeded))))))
  (format t ";; total contexts: ~D~%" total-contexts)
  (format t ";; success rate: ~D/~D~%" succeeded (length org-files)))
