(in-package :cl-user)
(defpackage codex.parser
  (:use :cl :trivial-types)
  (:export :parse-variable
           :parse-operator
           :parse-record)
  (:documentation "Given a system name, create a CommonDoc document from the
  documentation."))
(in-package :codex.parser)

(defun extract-and-concat-names (plist)
  (reduce #'(lambda (a b)
              (if b
                  ;; This puts a space between argument names
                  (concatenate 'string a " " b)
                  a))
          (loop for name in plist collecting
            (getf name :name))))

(defun parse-name (name-plist)
  "Parse a Quickdocs name plist."
  (getf (getf name-plist :symbol) :name))

(defun parse-documentation (plist)
  "Extract documentation from a Quickdocs plist."
  (getf plist :documentation))

(defun parse-lambda-list (plist)
  "Parse a Quickdocs lambda list."
  (let ((lambda-list (getf plist :lambda-list)))
    (extract-and-concat-names lambda-list)))

(defun parse-variable (variable-plist)
  "Parse a Quickdocs variable plist."
  (make-instance 'codex.macro:variable-node
                 :name (parse-name variable-plist)
                 :doc (parse-documentation variable-plist)))

(defun parse-operator (function-plist)
  "Parse a Quickdocs operator plist into a Codex macro node."
  (let ((class (case (getf function-plist :type)
                 (:function
                  'codex.macro:function-node)
                 (:macro
                  'codex.macro:macro-node)
                 (:generic
                  'codex.macro:generic-function-node)
                 (:method
                  'codex.macro:method-node))))
    (make-instance class
                   :name (parse-name function-plist)
                   :doc (parse-documentation function-plist)
                   :lambda-list (parse-lambda-list function-plist))))

(defun parse-record (record-plist)
  "Parse a structure or class into a Codex macro node."
  (flet ((parse-slot (slot-plist)
           (flet ((parse-methods (key)
                    (extract-and-concat-names (getf slot-plist key))))
             (make-instance 'codex.macro:slot-node
                            :name (parse-name slot-plist)
                            :doc (parse-documentation slot-plist)
                            :accessors (parse-methods :accessors)
                            :readers (parse-methods :readers)
                            :writers (parse-methods :writers)))))
    (let ((class (case (getf record-plist :type)
                   (:struct
                    'codex.macro:struct-node)
                   (:class
                    'codex.macro:class-node))))
      (make-instance class
                     :name (parse-name record-plist)
                     :doc (parse-documentation record-plist)
                     :slots
                     (loop for slot in (getf record-plist :slot-list) collecting
                       (parse-slot slot))))))
