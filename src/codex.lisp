(in-package :cl-user)
(defpackage codex
  (:use :cl)
  (:documentation "The main interface."))
(in-package :codex)

(defun document (system-name)
  "Generate documentation for a system."
  (let ((manifest-pathname
          (codex.manifest:system-manifest-pathname system-name)))
    (unless (probe-file manifest-pathname)
      (error 'codex.error:no-manifest :system-name system-name))
    (let ((manifest (codex.manifest:parse-manifest manifest-pathname)))
      ;; Now that we have the manifest, parse the systems it mentions and build
      ;; an index of documentation
      t)))
