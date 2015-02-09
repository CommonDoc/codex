(in-package :cl-user)
(defpackage codex-test-system
  (:use :cl)
  (:export :variable
           :func
           :mac)
  (:documentation "docstring"))
(in-package :codex-test-system)

(defparameter var t
  "docstring")

(defun func (x y z) t)

(defmacro mac (a b c) t)
