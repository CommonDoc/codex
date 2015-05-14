(defpackage codex.markup
  (:use :cl)
  (:export :*current-markup-format*
           :parse-string)
  (:documentation "Parsing files and docstrings."))
(in-package :codex.markup)

(defvar *current-markup-format* nil
  "The name of the markup format that will be used to parse docstrings and
 files. This is a keyword that is passed to Pandocl.")

(defun parse-string (string)
  "Parse a docstring into a documentation node using the current markup format."
  (pandocl:parse-string string *current-markup-format*))
