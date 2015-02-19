(in-package :codex.index)

(defclass index ()
  ((table :accessor table
          :initarg :table
          :initform (make-hash-table :test #'equal)
          :type hash-table
          :documentation "The index's internal hash table.")))

(defmethod add-node ((index index) node)
  "Add a node to an index."
  (setf (gethash (codex.macro:render-full-symbol (codex.macro:doc-symbol node))
                 (table index))
        node))

(defmethod get-node ((index index) symbol-string)
  "Find a node from its symbol string."
  (gethash symbol-string (table index)))

(defvar *current-index* nil
  "The current index. Used to inject docstrings into files.")

(defmacro with-index ((index) &rest body)
  "Set the *current-index* to index, then execute the body."
  `(let ((*current-index* ,index))
     ,@body))

(defun get-from-current-index (string)
  "Find a symbol in the current index. The symbol string can be in any case."
  (get-node *current-index*
            (string-upcase string)))
