(in-package :cl-user)
(defpackage codex-test-system
  (:use :cl)
  (:documentation "docstring"))
(in-package :codex-test-system)

(defparameter var t
  "docstring")

(defvar var2 t
  "docstring")

(defconstant const t
  "docstring")

(defun func (a b &optional (c "") (l 1))
  "docstring"
  (declare (ignore a b c l))
  t)

(defmacro mac (a b c)
  "docstring"
  (declare (ignore a b c))
  t)

(defstruct struct
  "docstring"
  a
  (b "test" :type string)
  (c 1))

(deftype custom-string (val)
  "docstring"
  (string= val "my-string"))

(defclass test-class ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "docstring")
   (second-slot :reader second-slot
                :reader s-slot
                :initarg :second-slot
                :documentation "docstring")
   (unexported-slot :reader unexported-slot
                    :initarg :unexported-slot
                    :documentation "docstring"))
  (:documentation "docstring"))

(defgeneric test-method (obj a)
  (:documentation "docstring"))

(defmethod test-method ((tc test-class) a)
  "docstring"
  (declare (ignore tc a))
  t)

(defmacro indirectly-define-function ()
  "docstring"
  `(defun hidden-function ()
     "docstring"
     t))

(indirectly-define-function)
