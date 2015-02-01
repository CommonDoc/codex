(in-package :cl-user)
(defpackage codex-test.parser
  (:use :cl :fiveam)
  (:export :parser))
(in-package :codex-test.parser)

;;; Samples of Quickdocs output

(defparameter +qd-function-sample+
  '(:type :function
    :symbol
    (:name "DOC" :package-name "CLACK.UTIL.DOC" :externalp t)
    :lambda-list
    ((:name "HEADER" :package-name "CLACK.UTIL.DOC" :externalp nil)
     (:name "&OPTIONAL" :package-name "COMMON-LISP" :externalp t)
     ((:name "STRING" :package-name "COMMON-LISP" :externalp t) "\"\"")
     ((:name "LEVEL" :package-name "CLACK.UTIL.DOC" :externalp nil) "1"))
    :documentation "Set documentation to current package"))

(defparameter +qd-variable-sample+
  '(:type :variable :symbol
    (:name "*CLACK-OUTPUT*" :package-name "CLACK" :externalp t) :documentation
    "Standard output for a Clack running process." :initial-value
    "'*standard-output*"))

(def-suite parser
  :description "Testing the Quickdics->Codex parser.")
(in-suite parser)
