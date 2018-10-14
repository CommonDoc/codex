(defpackage codex.macro
  (:use :cl)
  (:import-from :common-doc
                :content-node
                :text-node
                :code
                :list-item
                :unordered-list
                ;; Operators
                :define-node
                :children
                :text
                ;; Functions
                :make-text
                :make-meta
                :make-web-link)
  (:import-from :common-doc.macro
                :macro-node
                :expand-macro)
  (:export :*index*
           :cl-doc
           :with-package
           :param
           :spec
           :use-docstring)
  (:documentation "CommonDoc macros for Codex."))
(in-package :codex.macro)

;;; Variables

(defvar *index* nil
  "The current Docparser index.")

(defvar *current-package-name* nil
  "The name of the current package. Set by the with-package macro.")

;;; Macro classes

(define-node cl-doc (macro-node)
  ()
  (:tag-name "cl:doc")
  (:documentation "Insert documentation of a node."))

(define-node with-package (macro-node)
  ((name :reader package-macro-name
         :type string
         :attribute-name "name"
         :documentation "The package's name."))
  (:tag-name "cl:with-package")
  (:documentation "Set the current package to use in the body."))

(define-node param (macro-node)
  ()
  (:tag-name "cl:param")
  (:documentation "An argument of an operator."))

(define-node spec (macro-node)
  ()
  (:tag-name "cl:spec")
  (:documentation "Add a link to the Common Lisp HyperSpec."))

;;; Utilities

(defun make-class-metadata (class)
  "Create metadata for HTML classes."
  (make-meta
   (list
    (cons "html:class" (if (listp class)
                           (format nil "~{codex-~A~#[~:; ~]~}" class)
                           (concatenate 'string
                                        "codex-"
                                        class))))))

(defmethod name-node (node)
  "Create a node representing the name of a node. If it is a setf operation,
\"setf \" is automatically prepended before the name text."
  (make-instance 'code
                 :metadata (make-class-metadata "name")
                 :children (list
                            (make-text
                             (concatenate 'string
                                          (if (and (typep node 'docparser:operator-node)
                                                   (docparser:operator-setf-p node))
                                              "setf " "")
                                          (docparser:render-humanize
                                           (docparser:node-name node)))))))

(defun check-node-docstring (node)
  "Check and return the node's docstring.
If there is none, codex.error:no-docstring condition is signalled."
  (let ((docstring (docparser:node-docstring node)))
    (if docstring docstring
        (error 'codex.error:no-docstring :node node))))

(defun docstring-node (node)
  "Create a node representing a node's docstring."
  (make-instance 'content-node
                 :metadata (make-class-metadata "docstring")
                 :children
                 (restart-case
                     (children
                      (codex.markup:parse-string
                       (check-node-docstring node)))
                   (use-docstring (docstring)
                     :report "Enter a new docstring"
                     :interactive (lambda ()
                                    (format *query-io* "Enter a new docstring: ")
                                    (force-output *query-io*)
                                    (list (read-line *query-io*)))
                     (list (make-text docstring))))))

(defun list-to-code-node (class list)
  (make-instance 'code
                 :metadata (make-class-metadata class)
                 :children (list
                            (make-text
                             (with-standard-io-syntax
                               (let ((*print-case* :downcase))
                                 (princ-to-string list)))))))

(defun make-doc-node (classes &rest children)
  (make-instance 'content-node
                 :metadata (make-class-metadata (append (list "doc-node")
                                                        classes))
                 :children children))

;;; Docparser nodes to CommonDoc nodes

(defgeneric expand-node (node)
  (:documentation "Turn a Docparser node into a CommonDoc one."))

(defun expand-operator-node (node class-name)
  "Expand a generic operator node. Called by more specific methods."
  (make-doc-node (list "operator" class-name)
                 (name-node node)
                 (list-to-code-node "lambda-list"
                                    (docparser:operator-lambda-list node))
                 (docstring-node node)))

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

(defmethod expand-node ((node docparser:struct-slot-node))
  "Expand a structure slot node. This doesn't have any docstrings."
  (make-instance 'list-item
                 :metadata (make-class-metadata (list "slot" "structure-slot"))
                 :children
                 (list
                  (name-node node))))

(defmethod expand-node ((node docparser:class-slot-node))
  "Expand a class slot node."
  (make-instance 'list-item
                 :metadata (make-class-metadata (list "slot" "class-slot"))
                 :children
                 (list
                  (name-node node)
                  (docstring-node node))))

(defun expand-record-node (class node)
  (let ((beginning (list (list "record" class)
                         (name-node node)))
        (middle (when (typep node 'docparser:class-node)
                  (list (make-instance 'content-node
                                       :metadata (make-class-metadata "superclass-node")
                                       :children
                                       (list (make-instance 'code
                                                            :metadata
                                                            (make-class-metadata "superclass-label")
                                                            :children
                                                            (list (make-text "Superclasses:")))
                                             (list-to-code-node
                                              "superclass-list"
                                              (append (docparser:class-node-superclasses node)
                                                      (list t))))))))
        (end (list (docstring-node node)
                   (make-instance 'unordered-list
                                  :metadata (make-class-metadata "slot-list")
                                  :children
                                  (loop for slot in (docparser:record-slots node)
                                     collecting (expand-node slot))))))
    (apply #'make-doc-node (append beginning middle end))))

(defmethod expand-node ((node docparser:struct-node))
  "Expand a structure definition node."
  (expand-record-node "structure" node))

(defmethod expand-node ((node docparser:class-node))
  "Expand a class definition node."
  (expand-record-node "class" node))

(defmethod expand-node ((node docparser:condition-node))
  "Expand a condition definition node."
  (expand-record-node "condition" node))

(defmethod expand-node ((node docparser:variable-node))
  "Expand a variable node."
  (make-doc-node (list "variable")
                 (name-node node)
                 (docstring-node node)))

(defmethod expand-node ((node docparser:documentation-node))
  "Backup method when someone has created a subclass of documentation-node that's not
explicitly supported by this."
  (make-doc-node (list)
                 (name-node node)
                 (docstring-node node)))

(defmethod expand-node ((node docparser:type-node))
  "Expand a type node."
  (make-doc-node (list "type")
                 (name-node node)
                 (list-to-code-node "type-def"
                                    (docparser:operator-lambda-list node))
                 (docstring-node node)))

(defmethod expand-node ((node t))
  "When expanding an unsupported node, rather than generate an error, simply
create an error message."
  (make-text (format nil "Unsupported node type ~A." (type-of node))
             :metadata (make-class-metadata (list "error" "unsupported-node-error"))))

;;; Macroexpansions

(defparameter +type-name-to-class-map+
  (list (cons "function"      'docparser:function-node)
        (cons "setf-function" 'docparser:function-node)
        (cons "macro"         'docparser:macro-node)
        (cons "generic"       'docparser:generic-function-node)
        (cons "setf-generic"  'docparser:generic-function-node)
        (cons "method"        'docparser:method-node)
        (cons "setf-method"   'docparser:method-node)
        (cons "variable"      'docparser:variable-node)
        (cons "struct"        'docparser:struct-node)
        (cons "class"         'docparser:class-node)
        (cons "condition"     'docparser:condition-node)
        (cons "type"          'docparser:type-node)
        (cons "cfunction"     'docparser:cffi-function)
        (cons "ctype"         'docparser:cffi-type)
        (cons "cstruct"       'docparser:cffi-struct)
        (cons "cunion"        'docparser:cffi-union)
        (cons "cenum"         'docparser:cffi-enum)
        (cons "cbitfield"     'docparser:cffi-bitfield))
  "Associate the string names of Docparser classes to the corresponding
docparser class names.")

(defun find-node-type-by-name (name)
  (rest (assoc name +type-name-to-class-map+ :test #'equal)))

(defun node-not-found (symbol)
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "error" "no-node"))
                 :children
                 (list
                  (make-text "No node with name ")
                  (make-instance 'code
                                 :children
                                 (list
                                  (make-text (string-downcase symbol))))
                  (make-text "."))))

(defun no-such-type (name)
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "error" "no-type"))
                 :children
                 (list
                  (make-text "No node type with name ")
                  (make-instance 'code
                                 :children
                                 (list (make-text name)))
                  (make-text "."))))

(defun no-method-lambda-list (name)
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "error" "no-method-lambda-list"))
                 :children
                 (list
                  (make-text "Need a lambda list to find the method ")
                  (make-instance 'code
                                 :children
                                 (list (make-text (string-downcase name))))
                  (make-text "."))))

(defun no-method-found (name lambda-list)
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "error" "no-method-found"))
                 :children
                 (list
                  (make-text "No method ")
                  (make-instance 'code
                                 :children
                                 (list (make-text (string-downcase name))))
                  (make-text " with the lambda list ")
                  (make-instance 'code
                                 :children
                                 (list (make-text (string-downcase lambda-list))))
                  (make-text " found."))))

(defun find-node (type symbol rest)
  (let ((class (find-node-type-by-name type))
        (is-setf (alexandria:starts-with-subseq "setf-" type)))
    (if class
        ;; Use docparser to find suitable nodes and then filter out ones whose
        ;; are operator-nodes but their setfp slot doesn't match type (is-setf).
        (let ((nodes (remove-if #'(lambda (nd)
                                    (and (typep nd 'docparser:operator-node)
                                         (not (equal is-setf (docparser:operator-setf-p nd)))))
                                (docparser:query *index*
                                                 :package-name *current-package-name*
                                                 :symbol-name (string-upcase symbol)
                                                 :class class))))
          (if (> (length nodes) 0)
              (if (eq class 'docparser:method-node)
                  ;; Search for the proper method using the arglist
                  (if (equal rest (list ""))
                      (no-method-lambda-list symbol)
                      (let* ((lambda-list (princ-to-string
                                          (read-from-string
                                           (format nil "(~{~A~^ ~})" rest))))
                             (method (find-if #'(lambda (method)
                                                  (string= lambda-list
                                                           (princ-to-string
                                                            (docparser:operator-lambda-list
                                                             method))))
                                              nodes)))
                        (if method
                            (expand-node method)
                            (no-method-found symbol lambda-list))))
                  ;; Not a method.
                  (expand-node (elt nodes 0)))
              ;; No node with that name, report an error
              (node-not-found symbol)))
        (no-such-type type))))

(defmethod expand-macro ((node cl-doc))
  (let* ((text (common-doc.ops:collect-all-text node))
         (arguments (split-sequence:split-sequence #\Space text)))
    (destructuring-bind (type symbol &rest rest)
        arguments
      (format t "Inserting documentation for ~A ~S.~%" type symbol)
      (find-node type symbol rest))))

(defmethod expand-macro ((node with-package))
  (let* ((package-name (package-macro-name node))
         (*current-package-name* (string-upcase package-name))
         (new-node (make-instance 'content-node
                                  :children (children node))))
    ;; Expand inner macros
    (common-doc.macro:expand-macros new-node)
    new-node))

(defmethod expand-macro ((node param))
  (make-instance 'code
                 :metadata (make-class-metadata "param")
                 :children (children node)))

(defun url-for-symbol (symbol-name)
  "Return the Hyperspec or l1sp.org URL for a symbol in the CL package."
  (concatenate 'string "http://l1sp.org/cl/" symbol-name))

(defmethod expand-macro ((node spec))
  (make-web-link (url-for-symbol (text (first (children node))))
                 (list
                  (make-instance 'code
                                 :children (children node)))))
