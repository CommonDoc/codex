(in-package :cl-user)
(defpackage codex.error
  (:use :cl)
  ;; Classes
  (:export :codex-error
           :manifest-error
           :unsupported-output-format
           :template-error
           :no-docstring)
  ;; Accessors
  (:export :system-name
           :message
           :format-name
           :template-name
           :node)
  (:documentation "Codex errors."))
(in-package :codex.error)

(define-condition codex-error ()
  ()
  (:documentation "The base class of all Codex errors."))

(define-condition manifest-error (codex-error)
  ((system-name :reader system-name
                :initarg :system-name
                :type keyword
                :documentation "The name of the system.")
   (message :reader message
            :initarg :message
            :type string
            :documentation "The error message."))
  (:report
   (lambda (condition stream)
     (format stream "Manifest error for system ~A: ~A"
             (system-name condition)
             (message condition))))
  (:documentation "Signalled when there is an error parsing a manifest file."))

(define-condition unsupported-output-format (codex-error)
  ((format-name :reader format-name
                :initarg :format-name
                :type keyword
                :documentation "The name of the output format."))
  (:report
   (lambda (condition stream)
     (format stream "Unsupported output format: ~A." (format-name condition))))
  (:documentation "Signalled when Codex doesn't know the output format a
  document specifies."))

(define-condition template-error (codex-error)
  ((template-name :reader template-name
                  :initarg :template-name
                  :type keyword
                  :documentation "The template name.")
   (message :reader message
            :initarg :message
            :type string
            :documentation "The error message."))
  (:report
   (lambda (condition stream)
     (format stream "Error with template ~A: ~A."
             (template-name condition)
             (message condition))))
  (:documentation "Signalled by errors related to templates."))


(define-condition no-docstring (codex-error)
  ((node :reader node
         :initarg :node
         :documentation "The node without docstring."))
  (:report
   (lambda (c s)
     (let ((node (node c)))
       (format s "No docstring in node ~a(~a)"
               node (docparser:node-name node)))))
  (:documentation "Signalled when a node has no docstring."))
