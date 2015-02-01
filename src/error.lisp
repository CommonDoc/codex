(in-package :cl-user)
(defpackage codex.error
  (:use :cl)
  (:export :codex-error
           :no-manifest
           :system-name)
  (:documentation "Codex errors."))
(in-package :codex.error)

(define-condition codex-error ()
  ()
  (:documentation "The base class of all Codex errors."))

(define-condition no-manifest (codex-error)
  ((system-name :reader system-name
                :initarg :system-name
                :type keyword
                :documentation "The name of the system."))
  (:documentation "Signalled when a system has no manifest file."))
