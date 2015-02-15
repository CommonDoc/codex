(in-package :cl-user)
(defpackage codex.markup
  (:use :cl)
  (:export :with-markup
           :parse-string)
  (:documentation "Parsing files and docstrings."))
(in-package :codex.markup)

(defparameter +known-markup-formats+
  (let ((table (make-hash-table)))
    (setf (gethash :vertex table) (make-instance 'vertex:vertex))
    table)
  "A hash table of the known markup formats.")

(defun validate-markup-format (format-name)
  "Returns t if format-name is known, signals unsupported-markup-format
otherwise."
  (if (gethash format-name +known-markup-formats+)
      t
      (error 'codex.error:unsupported-markup-format :format-name format-name)))

(defvar *current-markup-format* :vertex
  "The markup format that will be used to parse docstrings and files.")

(defmacro with-markup ((format-name) &rest body)
  "Execute body with a markup format."
  `(when (validate-markup-format ,format-name)
     (let ((*current-markup-format* ,format-name))
       ,@body)))

(defun parse-string (string)
  "Parse a docstring or file string into a documentation node."
  (common-doc.format:parse-document
   (gethash *current-markup-format* +known-markup-formats+)
   string))
