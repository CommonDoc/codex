(in-package :codex)

(defun document (system-name)
  "Generate documentation for a system."
  (let ((manifest-pathname
          (codex.manifest:system-manifest-pathname system-name)))
    (unless (probe-file manifest-pathname)
      (error 'codex.error:no-manifest :system-name system-name))
    (let* ((manifest (codex.manifest:parse-manifest manifest-pathname)))
      (codex.build:build-manifest manifest
                                  (uiop:pathname-directory-pathname manifest-pathname)))))
