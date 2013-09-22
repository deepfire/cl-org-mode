(defpackage :cl-org-mode-extended
  (:use :common-lisp :alexandria :iterate
        :cl-org-mode :cl-org-mode-slots :cl-org-mode-utils)
  (:import-from :cl-org-mode-raw
                :strconcat :strconcat* :unzip)
  (:export
   ;;
   #:org-dress-error
   #:org-object-error
   #:org-object-simple-error
   #:org-object-warning
   #:org-object-simple-warning
   #:org-object-relink-error
   ;;
   #:hash-of
   #:with-hash-cache
   #:clear-hash-cache
   ;;
   #:org-parse-extended))
