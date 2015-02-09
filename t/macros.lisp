(in-package :cl-user)
(defpackage codex-test.macros
  (:use :cl :fiveam)
  (:import-from :common-doc.util
                :make-text)
  (:import-from :common-doc.macro
                :expand-macro)
  (:import-from :common-doc
                :document-link
                :text-node
                :list-item
                :definition
                :unordered-list
                :definition-list
                :document-reference
                :section-reference)
  (:export :macroexpansions))
(in-package :codex-test.macros)

;;; Utilities

(defun parse-vertex (input)
  (common-doc.format:parse-document (make-instance 'vertex:vertex)
                                    input))

(defun test-equal (vertex-input vertex-output)
  (common-doc.ops:node-equal
   (expand-macro
    (parse-vertex vertex-input))
   (parse-vertex vertex-output)))

(defun expand-print (node)
  (print
   (common-doc.format:emit-to-string
    (make-instance 'common-html:html)
    (expand-macro node))))

;;; Suite

(def-suite macroexpansions
  :description "Codex macro tests.")
(in-suite macroexpansions)

(test cl-ref
  (is-true
   (test-equal "\\clref{sym}"
               "\\ref[doc=common-lisp, sec=symbol-sym]"))
  (is-true
   (test-equal "\\clref{pack:sym}"
               "\\ref[doc=pack, sec=symbol-sym]")))

;(test function
;  (finishes
;    (expand-print (make-instance 'codex.macro:function-node
;                                 :name "f"
;                                 :doc (make-text "docstring")
;                                 :lambda-list (list "a" "b" "c")))))
