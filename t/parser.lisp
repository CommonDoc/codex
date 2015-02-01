(in-package :cl-user)
(defpackage codex-test.parser
  (:use :cl :fiveam)
  (:import-from :codex.macro
                :doc-name
                :doc-description
                :operator-lambda-list)
  (:export :parser))
(in-package :codex-test.parser)

;;; Samples of Quickdocs output

(defparameter +qd-function-sample+
  '(:type :function
    :symbol
    (:name "FOO-FUN" :package-name "FOO-PACKAGE" :externalp t)
    :lambda-list
    ((:name "HEADER" :package-name "FOO-PACKAGE" :externalp nil)
     (:name "&OPTIONAL" :package-name "COMMON-LISP" :externalp t)
     ((:name "STRING" :package-name "COMMON-LISP" :externalp t) "\"\"")
     ((:name "LEVEL" :package-name "FOO-PACKAGE" :externalp nil) "1"))
    :documentation "Docstring"))

(defparameter +qd-variable-sample+
  '(:type :variable
    :symbol
    (:name "FOO-VAR" :package-name "FOO-PACKAGE" :externalp t) :documentation
    "Docstring"))

(def-suite parser
  :description "Testing the Quickdics->Codex parser.")
(in-suite parser)

(test variable
  (let ((var (codex.parser:parse-variable +qd-variable-sample+)))
    (is
     (equal (doc-name var)
            "FOO-VAR"))
    (is
     (equal (doc-description var)
            "Docstring"))))

(test function
  (let ((func (codex.parser:parse-operator +qd-function-sample+)))
    (is
     (equal (doc-name func)
            "FOO-FUN"))
    (is
     (equal (operator-lambda-list func)
            "HEADER &OPTIONAL (STRING \"\") (LEVEL 1)"))
    (print (operator-lambda-list func))
    (is
     (equal (doc-description func)
            "Docstring"))))
