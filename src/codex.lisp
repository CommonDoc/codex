(in-package :cl-user)
(defpackage codex
  (:use :cl)
  (:import-from :codex.manifest
                :document
                :document-title
                :document-sources
                :manifest
                :project-name
                :markup-format
                :systems
                :documents)
  (:documentation "The main interface."))
(in-package :codex)

(defgeneric build (object)
  (:documentation "Build a document or a manifest."))

(defmethod build ((document document))
  "Build a document."
  t)

(defmethod build ((manifest manifest))
  "Build a manifest."
  (let ((index (codex.index:with-index (index)
                 (loop for system-name in (systems manifest) do
                   (codex.parser:parse-system-into-index index system-name)))))
    ;; Now that we have the index, go through the list of documents, building
    ;; each one
    (loop for document in (documents manifest) do
      (build document))))

(defun document (system-name)
  "Generate documentation for a system."
  (let ((manifest-pathname
          (codex.manifest:system-manifest-pathname system-name)))
    (unless (probe-file manifest-pathname)
      (error 'codex.error:no-manifest :system-name system-name))
    (let* ((manifest (codex.manifest:parse-manifest manifest-pathname)))
      (build manifest))))
