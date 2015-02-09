(in-package :cl-user)
(defpackage codex.parser
  (:use :cl :trivial-types)
  (:import-from :codex.index
                :add-node)
  (:export :parse-variable
           :parse-operator
           :parse-record
           :parse-system-into-index)
  (:documentation "Given a system name, create a CommonDoc document from the
  documentation."))
(in-package :codex.parser)

(defun extract-and-concat-names (plist)
  (reduce #'(lambda (a b)
              (if b
                  ;; This puts a space between argument names
                  (concatenate 'string a " " b)
                  a))
          (loop for name-plist in plist collecting
            (if (listp (first name-plist))
                ;; Optional value
                (format nil "(~A ~A)"
                        (getf (first name-plist) :name)
                        (second name-plist))
                ;; Regular argument
                (getf name-plist :name)))))

(defun parse-name (name-plist)
  "Parse a Quickdocs name plist into a string."
  (let ((name-plist (getf name-plist :symbol)))
    (concatenate 'string
                 (getf name-plist :package-name)
                 ":"
                 (getf name-plist :name))))

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
                     (loop for slot in (getf record-plist :slot-list)
                           collecting
                           (parse-slot slot))))))

(defun parse-symbol (symbol-plist)
  "Parse an arbitrary Quickdocs symbol plist into a Codex macro node."
  (let ((type (getf symbol-plist :tye)))
    (cond
      ((eq type :variable)
       (parse-variable symbol-plist))
      ((member type (list :struct :class))
       (parse-record symbol-plist))
      ((member type (list :function :macro :generic :method))
       (parse-operator symbol-plist))
      (t
       (error "Unknown symbol type.")))))

(defun parse-system-into-index (index system-name)
  "Load a system, extract all its documentation, parse it, and store it in an
index."
  (let* ((system-plist (quickdocs.parser:parse-documentation system-name))
         (packages (getf system-plist :package-list)))
    (loop for package in packages do
      (let ((package-name (getf package :full-name))
            (symbols (getf package :symbol-list)))
        (loop for symbol in symbols do
          (add-node index (parse-symbol symbol))))))
  t)
