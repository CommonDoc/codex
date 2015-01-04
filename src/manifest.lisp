(in-package :cl-user)
(defpackage codex.manifest
  (:use :cl)
  (:documentation "Parsing Codex manifest files."))
(in-package :codex.manifest)

(defun system-manifest-pathname (system-name)
  "Return the path to a system's manifest file."
  (merge-pathnames #p"docs/manifest.lisp-expr"
                   (asdf:component-pathname
                    (asdf:find-system system-name))))

(defun read-manifest (pathname)
  "Read a manifest file into an S-expression."
  (with-open-file (input-stream pathname
                                :direction :input)
    (read input-stream)))
