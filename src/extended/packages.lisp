(defpackage :cl-org-mode-extended
  (:use :common-lisp :alexandria :iterate
        :cl-org-mode :cl-org-mode-slots)
  (:import-from :cl-org-mode-raw
                :strconcat :strconcat* :unzip)
  (:export
   ;;
   #:hash-of
   ;;
   #:org-parse-extended))
