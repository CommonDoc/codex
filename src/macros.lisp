(in-package :cl-user)
(defpackage codex.macros
  (:use :cl)
  (:import-from :common-doc
                :<document-link>
                :<text-node>
                :define-node
                :children
                :text)
  (:import-from :common-doc.macro
                :<macro-node>
                :expand-macro))
(in-package :codex.macros)

(define-node <cl-ref> (<macro-node>)
  ()
  (:tag-name "clref")
  (:documentation "A reference to a Common Lisp symbol."))

(defmethod expand-macro ((ref <cl-ref>))
  (let ((text-node (elt (children ref) 0)))
    (assert (typep text-node '<text-node>))
    (let* ((symbol (text text-node))
           (colon-pos (position #\: symbol))
           (package-name (if colon-pos
                             (subseq symbol 0 colon-pos)))
           (symbol-name (if colon-pos
                            (subseq symbol (1+ colon-pos))
                            symbol)))
      (make-instance '<document-link>
                     :document-reference package-name
                     :section-reference (concatenate 'string
                                                     "sym-" symbol-name)))))
