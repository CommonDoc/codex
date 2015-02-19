
(in-package :codex.error)

(define-condition codex-error ()
  ()
  (:documentation "The base class of all Codex errors."))

(define-condition no-manifest (codex-error)
  ((system-name :reader system-name
                :initarg :system-name
                :type keyword
                :documentation "The name of the system."))
  (:report
   (lambda (condition stream)
     (format stream "No manifest file for system ~A." (system-name condition))))
  (:documentation "Signalled when a system has no manifest file."))

(define-condition unsupported-markup-format (codex-error)
  ((format-name :reader format-name
                :initarg :format-name
                :type keyword
                :documentation "The name of the markup format."))
  (:report
   (lambda (condition stream)
     (format stream "Unsupported markup format: ~A." (format-name condition))))
  (:documentation "Signalled when Codex doesn't know the markup format a
  manifest specifies."))
