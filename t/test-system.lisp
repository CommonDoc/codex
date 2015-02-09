(in-package :cl-user)
(defpackage codex-test-system
  (:use :cl)
  (:export :var
           :func
           :mac
           :rec1
           :rec2
           :struct-slot-a
           :struct-slot-b
           :test-class
           :first-slot
           :second-slot
           :test-method)
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

(defstruct rec1
  "docstring"
  a
  (b "test" :type string)
  (c 1))

(defstruct rec2
  "docstring"
  struct-slot-a
  struct-slot-b)

(defclass test-class ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "docstring")
   (second-slot :reader second-slot
                :initarg :second-slot
                :documentation "docstring")
   (unexported-slot :reader unexported-slot
                 :initarg :unexported-slot
                 :documentation "docstring"))
  (:documentation "docstring"))

(defmethod test-method ((tc test-class) a)
  "docstring"
  t)
