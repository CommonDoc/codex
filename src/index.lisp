(in-package :cl-user)
(defpackage codex.index
  (:use :cl)
  (:export :index
           :add-node
           :get-node
           :with-index)
  (:documentation "An index is a data structure that associates symbol names to
  their CommonDoc nodes so they can be included in documentation output."))
(in-package :codex.index)

(defclass index ()
  ((table :accessor table
          :initarg :table
          :initform (make-hash-table :test #'equal)
          :type hash-table
          :documentation "The index's internal hash table.")))

(defmethod add-node ((index index) node)
  "Add a node to an index."
  (setf (gethash (codex.macro:doc-name node) (table index)) node))

(defmethod get-node ((index index) symbol-string)
  "Find a node from its symbol string."
  (gethash symbol-string (table index)))

(defmacro with-index ((index) &rest body)
  "Create an index, execute body (Which should fill it), and return it."
  `(let ((,index (make-instance 'index)))
     ,@body
     ,index))
