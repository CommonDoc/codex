(in-package :cl-user)
(defpackage codex-test.parser
  (:use :cl :fiveam)
  (:import-from :codex.macro
                :doc-symbol
                :render-full-symbol
                :doc-description
                :operator-lambda-list
                :record-slots)
  (:export :parser))
(in-package :codex-test.parser)

;;; Utilities

(defparameter *index* (make-instance 'codex.index:index)
  "The index of Codex nodes.")

(defun sym (string)
  (format nil "CODEX-TEST-SYSTEM:~A" string))

(defun node-name (node)
  (render-full-symbol (doc-symbol node)))

(defmacro with-node-in-index ((var name) &rest body)
  `(let ((,var (codex.index:get-node *index* (sym ,name))))
     (is
      (equal (node-name ,var)
             (sym ,name)))
     (is
      (equal (common-doc:text (doc-description ,var))
             "docstring"))
     ,@body))

;;; Tests)

(def-suite parser
  :description "Testing the Quickdics->Codex parser.")
(in-suite parser)

(test parse-system
  (is-true
   (codex.parser:parse-system-into-index *index* :codex-test-system)))

(test (parse-variable :depends-on parse-system)
  (with-node-in-index (var "VAR")
    t))

(test (parse-function :depends-on parse-system)
  (with-node-in-index (func "FUNC")
    (is
     (equal (operator-lambda-list func)
            "A B &OPTIONAL (C \"\") (L 1)"))))

(test (parse-macro :depends-on parse-system)
  (with-node-in-index (mac "MAC")
    (is
     (equal (operator-lambda-list mac)
            "A B C"))))

(test (parse-unexported-struct :depends-on parse-system)
  (with-node-in-index (rec "REC1")
    t))

(test (parse-exported-struct :depends-on parse-system)
  (with-node-in-index (rec "REC2")
    (is
     (equal (node-name (first (record-slots rec)))
            (sym "STRUCT-SLOT-A")))
    (is
     (equal (node-name (second (record-slots rec)))
            (sym "STRUCT-SLOT-B")))))

(test (parse-class :depends-on parse-system)
  (with-node-in-index (class "TEST-CLASS")
    (is
     (equal (length (record-slots class)) 2))
    (destructuring-bind (first-slot second-slot)
        (record-slots class)
      (is
       (equal (node-name first-slot)
              (sym "FIRST-SLOT")))
      (is
       (equal (node-name second-slot)
              (sym "SECOND-SLOT"))))))

(test (parse-method :depends-on parse-system)
  (with-node-in-index (meth "TEST-METHOD")
    (is
     (equal (operator-lambda-list meth)
            "(TC TEST-CLASS) A"))))
