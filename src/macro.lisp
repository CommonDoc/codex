(defpackage codex.macro
  (:use :cl)
  (:import-from :common-doc
                :content-node
                :text-node
                ;; Operators
                :define-node
                :children
                :text)
  (:import-from :common-doc.macro
                :macro-node
                :expand-macro)
  (:import-from :common-doc.util
                :make-text
                :make-meta)
  (:export :*index*
           :cl-doc
           :param)
  (:documentation "CommonDoc macros for Codex."))
(in-package :codex.macro)

;;; Variables

(defvar *index* nil
  "The current Docparser index.")

;;; Macro classes

(define-node cl-doc (macro-node)
  ()
  (:tag-name "cldoc")
  (:documentation "Insert parsed documentation."))

(define-node param (macro-node)
  ()
  (:tag-name "param")
  (:documentation "An argument of an operator."))

;;; Utilities

(defun make-class-metadata (class)
  "Create metadata for HTML classes."
  (make-meta
   (list
    (cons "class" (if (listp class)
                      (format nil "~{codex-~A~#[~:; ~]~}" class)
                      (concatenate 'string
                                   "codex-"
                                   class))))))

(defun parse-symbol-string (string)
  "Parse a symbol string into a list of two values: Package name (string) and
symbol name (string)."
  (let* ((colon-pos (position #\: string))
         (package-name (subseq string 0 colon-pos))
         (symbol-name (subseq string (1+ colon-pos))))
    (list package-name symbol-name)))

;;; Docparser nodes to CommonDoc nodes

(defgeneric expand-node (node)
  (:documentation "Turn a Docparser node into a CommonDoc one."))

(defmethod expand-node ((node docparser:symbol-node))
  "Exand a symbol node."
  (make-text (docparser:render-humanize node)
             (make-class-metadata "symbol")))

(defun expand-operator-node (node class-name)
  "Expand a generic operator node. Called by more specific methods."
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "doc-node" class-name))
                 :children
                 (list
                  (make-instance 'code
                                 :metadata (make-class-metadata "name")
                                 :children (list
                                            (expand-node (docparser:node-name node))))
                  (make-instance 'code
                                 :metadata (make-class-metadata "lambda-list")
                                 :children (list
                                            (make-text
                                             (with-standard-io-syntax
                                               (let ((*print-case* :downcase))
                                                 (prin1-to-string
                                                  (docparser:operator-lambda-list node)))))))
                  (make-instance 'content-node
                                 :metadata (make-class-metadata "docstring")
                                 :children (list
                                            (docparser:node-docstring node))))))

(defmethod expand-node ((node docparser:function-node))
  "Expand a function node."
  (expand-operator-node node "function"))

(defmethod expand-node ((node docparser:macro-node))
  "Expand a macro node."
  (expand-operator-node node "macro"))

(defmethod expand-node ((node docparser:generic-function-node))
  "Expand a generic function node."
  (expand-operator-node node "generic-function"))

(defmethod expand-node ((node docparser:method-node))
  "Expand a method node."
  (expand-operator-node node "method"))

(defmethod expand-node ((node docparser:operator-node))
  "Backup method when someone has created a subclass of operator-node that's not
explicitly supported by this method."
  (expand-operator-node node "operator"))

(defmethod expand-node ((node t))
  "When expanding an unsupported node, rather than generate an error, simply
create an error message."
  (make-text (format nil "Unsupported node type ~A." (type-of node))
             (make-class-metadata (list "error" "unsupported-node-error"))))

;;; Macroexpansions

(defmethod expand-macro ((cl-doc cl-doc))
  (let ((text-node (elt (children cl-doc) 0)))
    (assert (typep text-node 'text-node))
    (destructuring-bind (package symbol)
        (parse-symbol-string (text text-node))
      (format t "Inserting documentation for symbol ~S.~%" symbol)
      ;; Extract the node from the index
      (let ((nodes (docparser:query *index*
                                    :package-name (string-upcase package)
                                    :symbol-name (string-upcase symbol))))
        (if (> (length nodes) 0)
            (first nodes)
            ;; No node with that name, report an error
            (make-instance 'content-node
                           :metadata (make-class-metadata (list "error" "no-node"))
                           :children
                           (list
                            (make-text "No node with name ")
                            (make-instance 'code
                                           :children
                                           (list
                                            (make-text (string-downcase symbol))))
                            (make-text "."))))))))

(defmethod expand-macro ((param param))
  (make-instance 'content-node
                 :metadata (make-class-metadata "param")
                 :children (children param)))
