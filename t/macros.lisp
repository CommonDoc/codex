(in-package :cl-user)
(defpackage codex-test.macros
  (:use :cl :fiveam)
  (:import-from :common-doc.util
                :make-text)
  (:import-from :common-doc.macro
                :expand-macro)
  (:import-from :common-doc
                :<document-link>
                :<text-node>
                :<list-item>
                :<definition>
                :<unordered-list>
                :<definition-list>
                :document-reference
                :section-reference)
  (:export :tests))
(in-package :codex-test.macros)

;;; Utilities

(defun test-equal (vertex-input vertex-output)
  (common-doc.ops:node-equal
   (expand-macro
    (common-doc.format:parse-document (make-instance 'vertex:<vertex>)
                                      vertex-input))
    (common-doc.format:parse-document (make-instance 'vertex:<vertex>)
                                      vertex-output)))

;;; Suite

(def-suite tests
  :description "Codex macro tests.")
(in-suite tests)

(test cl-ref
  (is-true
   (test-equal "\\clref{sym}"
               "\\ref[doc=common-lisp, sec=symbol-sym]"))
  (is-true
   (test-equal "\\clref{pack:sym}"
               "\\ref[doc=pack, sec=symbol-sym]")))
