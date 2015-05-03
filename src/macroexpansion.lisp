(in-package :codex.macro)

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



(defmethod expand-macro ((variable variable-node))
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "doc-node" "variable"))
                 :children
                 (list (make-text (render-humanize (doc-symbol variable))
                                  (make-class-metadata "name"))
                       (make-instance 'content-node
                                      :metadata (make-class-metadata "docstring")
                                      :children (list (doc-description variable))))))

(defmethod expand-macro ((slot slot-node))
  (labels ((list-of-methods-to-list (methods)
             (make-instance
              'unordered-list
              :children
              (loop for method in methods collecting
                (make-instance
                 'list-item
                 :children
                 (list
                  (make-instance
                   'code
                   :children
                   (list
                    (make-text (render-humanize method)))))))))
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
                                             :children slot-methods))
           (slot-name (make-instance 'code
                                     :metadata (make-class-metadata "slot-name")
                                     :children
                                     (list
                                      (make-text (render-humanize (doc-symbol slot)))))))
      (make-instance
       'definition-list
       :metadata (make-class-metadata "slot")
       :children
       (list
        (make-instance
         'definition
         :term (list slot-name)
         :definition
         (list
          (make-instance
           'content-node
           :metadata (make-class-metadata "docstring")
           :children (list (doc-description slot)))
          slot-methods-node)))))))

(defun expand-record-macro (instance class-metadata)
  (make-instance 'content-node
                 :metadata (make-class-metadata (list "doc-node" class-metadata))
                 :children
                 (list (make-instance 'code
                                      :metadata (make-class-metadata "name")
                                      :children
                                      (list
                                       (make-text
                                        (render-humanize (doc-symbol instance)))))
                       (make-instance 'content-node
                                      :metadata (make-class-metadata "docstring")
                                      :children (list (doc-description instance)))
                       (let ((slots (record-slots instance)))
                         (if slots
                             (make-instance 'content-node
                                            :metadata (make-class-metadata "slots")
                                            :children slots)
                             (make-instance 'content-node
                                            :metadata (make-class-metadata (list "warning" "no-slots"))
                                            :children
                                            (list
                                             (make-text "No slots."))))))))

(defmethod expand-macro ((struct struct-node))
  (expand-record-macro struct "struct"))

(defmethod expand-macro ((class class-node))
  (expand-record-macro class "class"))

(defmethod expand-macro ((type type-node))
  (expand-operator-macro type "type"))
