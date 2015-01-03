(in-package :cl-user)
(defpackage codex.macros
  (:use :cl)
  (:import-from :common-doc
                :define-node)
  (:import-from :common-doc.macro
                :<macro-node>
                :expand-macro))
(in-package :codex.macros)

(define-node <cl-ref> (<macro-node>)
  ()
  (:tag-name "clref")
  (:documentation "A reference to a Common Lisp symbol."))

(defmethod expand-macro ((ref <cl-ref>))
  nil)
