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

(def-suite tests
  :description "Codex macro tests.")
(in-suite tests)

(test cl-ref
  (let* ((ref (make-instance 'codex.macros:<cl-ref>
                             :children
                             (list
                              (make-text "test"))))
         (expanded-ref))
    (finishes
      (setf expanded-ref (expand-macro ref)))
    (is
     (typep expanded-ref '<document-link>))
    (is
     (equal (document-reference expanded-ref) "package-common-lisp"))
    (is
     (equal (section-reference expanded-ref) "symbol-test")))
  (let* ((ref (make-instance 'codex.macros:<cl-ref>
                             :children
                             (list
                              (make-text "pack:sym"))))
         (expanded-ref))
    (finishes
      (setf expanded-ref (expand-macro ref)))
    (is
     (typep expanded-ref '<document-link>))
    (is
     (equal (document-reference expanded-ref) "package-pack"))
    (is
     (equal (section-reference expanded-ref) "symbol-sym"))))
