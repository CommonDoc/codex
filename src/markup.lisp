(defpackage codex.markup
  (:use :cl)
  (:export :with-markup
           :parse-string)
  (:documentation "Parsing files and docstrings."))
(in-package :codex.markup)

(defvar *current-markup-format* nil
  "The name of the markup format that will be used to parse docstrings and
 files. This is a keyword that is passed to Pandocl.")

(defmacro with-markup ((format-name) &rest body)
  "Execute body with a markup format."
  `(let ((*current-markup-format* ,format-name))
     ,@body))

(defun parse-string (string)
  "Parse a docstring into a documentation node using the current markup format."
  (pandocl:parse-string string *current-markup-format*))
