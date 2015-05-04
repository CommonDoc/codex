(in-package :cl-user)
(defpackage codex-test-system
  (:use :cl)
  (:documentation "docstring"))
(in-package :codex-test-system)

(defparameter var t
  "docstring")

(defun func (a b &optional (c "") (l 1))
  "docstring"
  t)

(defmacro mac (a b c)
  "docstring"
  t)