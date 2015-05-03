(defpackage codex
  (:use :cl)
  (:export :document)
  (:documentation "The main interface."))
(in-package :codex)

(defun document (system-name)
  "Generate documentation for a system."
  (let ((manifest-pathname (codex.manifest:system-manifest-pathname system-name)))
    (unless (probe-file manifest-pathname)
      (error 'codex.error:manifest-error
             :system-name system-name
             :message "No manifest."))
    (let ((manifest (codex.manifest:parse-manifest manifest-pathname))
          (directory (uiop:pathname-directory-pathname manifest-pathname)))
      (codex.build:build-manifest manifest directory))))
