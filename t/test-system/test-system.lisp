(in-package :cl-user)
(defpackage codex-test-system
  (:use :cl)
  (:documentation "docstring"))
(in-package :codex-test-system)

;;; Common Lisp nodes

(defun func (a b &optional (c "") (l 1))
  "docstring"
  (declare (ignore a b c l))
  t)

(defmacro mac (a b c)
  "docstring"
  (declare (ignore a b c))
  t)

(defparameter var t
  "docstring")

(defstruct struct
  "docstring"
  a
  (b "test" :type string)
  (c 1))

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

(define-condition my-error ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "docstring"))
  (:documentation "docstring"))

(deftype custom-string (val)
  "docstring"
  (string= val "my-string"))

;;; CFFI nodes

(cffi:defcfun printf :int
  "docstring"
  (control :string) &rest)

(cffi:defctype size-t :unsigned-long "docstring")

(cffi:defcstruct cstruct
  "docstring"
  (a :int)
  (b :double))

(cffi:defcunion cunion
  "docstring"
  (a :int)
  (b :double))

(cffi:defcenum nums "docstring" :a :b (:c 3))

(cffi:defbitfield bits "docstring" :a :b (:c #x0200))
