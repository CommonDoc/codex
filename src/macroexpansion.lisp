(in-package :codex.macro)

(defmethod expand-macro ((sym symbol-node))
  (make-text (render-humanize sym)
             (make-class-metadata "symbol")))

(defun parse-symbol-string (string)
  (let* ((colon-pos (position #\: string))
         (package-name (subseq string 0 colon-pos))
         (symbol-name (subseq string (1+ colon-pos))))
    (list package-name symbol-name)))

(defmethod expand-macro ((ref cl-ref))
  (let ((text-node (elt (children ref) 0)))
    (assert (typep text-node 'text-node))
    (destructuring-bind (package-name symbol-name)
        (parse-symbol-string (text text-node))
      (make-instance 'document-link
                     :section-reference (concatenate 'string
                                                     "symbol-"
                                                     package-name
                                                     ":"
                                                     symbol-name)))))

(defmethod expand-macro ((cl-doc cl-doc))
  (let ((text-node (elt (children cl-doc) 0)))
    (assert (typep text-node 'text-node))
    (let ((symbol-string (string-upcase (text text-node))))
      (format t "Inserting documentation for symbol ~S.~%" symbol-string)
      ;; Extract the node from the index
      (let ((node (codex.index:get-from-current-index symbol-string)))
        (if node
            node
            ;; No node with that name, report an error
            (make-text (format nil "No node with name ~A." symbol-string)
                       (make-class-metadata "error no-node")))))))

(defmethod expand-macro ((param param))
  (make-instance 'content-node
                 :metadata (make-class-metadata "param")
                 :children (children param)))

(defun expand-operator-macro (instance class-name)
  (make-instance 'content-node
                 :metadata (make-class-metadata class-name)
                 :children
                 (list
                  (make-text (render-humanize (doc-symbol instance))
                             (make-class-metadata "name"))
                  (make-instance 'content-node
                                 :metadata (make-class-metadata "lambda-list")
                                 :children (list
                                            (make-text
                                             (operator-lambda-list instance))))
                  (make-instance 'content-node
                                 :metadata (make-class-metadata "docstring")
                                 :children (list
                                            (doc-description instance))))))

(defmethod expand-macro ((function function-node))
  (expand-operator-macro function "function"))

(defmethod expand-macro ((macro macro-node))
  (expand-operator-macro macro "macro"))

(defmethod expand-macro ((generic-function generic-function-node))
  (expand-operator-macro generic-function "generic-function"))

(defmethod expand-macro ((method method-node))
  (expand-operator-macro method "method"))

(defmethod expand-macro ((variable variable-node))
  (make-instance 'content-node
                 :metadata (make-class-metadata "variable")
                 :children
                 (list (make-text (render-humanize (doc-symbol variable))
                                  (make-class-metadata "name"))
                       (make-instance 'content-node
                                      :metadata (make-class-metadata "docstring")
                                      :children (list (doc-description variable))))))

(defmethod expand-macro ((slot slot-node))
  (labels ((list-of-methods-to-list (methods)
             (make-instance 'unordered-list
                            :children
                            (loop for method in methods collecting
                              (make-instance 'list-item
                                             :children
                                             (list (make-text method))))))
           (make-definition (slot-name text)
             (when (slot-value slot slot-name)
               (make-instance 'definition
                              :term (make-text text)
                              :definition (list-of-methods-to-list
                                           (slot-value slot slot-name))))))
    (let* ((accessors-definition (make-definition 'accessors "Accessors"))
           (readers-definition (make-definition 'readers "Readers"))
           (writers-definition (make-definition 'writers "Writers"))
           (slot-methods (remove-if #'null (list accessors-definition
                                                 readers-definition
                                                 writers-definition)))
           (slot-methods-node (make-instance 'definition-list
                                             :metadata (make-class-metadata "slot-methods")
                                             :children slot-methods)))
      (make-instance 'content-node
                     :metadata (make-class-metadata "slot")
                     :children
                     (list (doc-description slot)
                           slot-methods-node)))))

(defun expand-record-macro (instance class-metadata)
  (make-instance 'content-node
                 :metadata (make-class-metadata class-metadata)
                 :children
                 (list (make-text (render-humanize (doc-symbol instance))
                                  (make-class-metadata "name"))
                       (make-instance 'content-node
                                      :metadata (make-class-metadata "docstring")
                                      :children (list (doc-description instance)))
                       (record-slots instance))))

(defmethod expand-macro ((struct struct-node))
  (expand-record-macro struct "struct"))

(defmethod expand-macro ((class class-node))
  (expand-record-macro class "class"))

(defmethod expand-macro ((type type-node))
  (expand-operator-macro type "type"))
