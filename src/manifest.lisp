(in-package :cl-user)
(defpackage codex.manifest
  (:use :cl :trivial-types)
  (:documentation "Parsing Codex manifest files."))
(in-package :codex.manifest)

(defpackage :codex-manifest-user
  (:use :cl :codex.manifest)
  (:documentation "The package in which Codex manifests are read."))

(defclass manifest ()
  ((project-name :reader project-name
                 :initarg :project-name
                 :type string
                 :documentation "The project's name.")
   (markup-format :reader markup-format
                  :initarg :markup-format
                  :type keyword
                  :documentation "The markup format used in docstrings.")
   (packages :reader packages
             :initarg :packages
             :type (proper-list string)
             :documentation "A list of packages to document."))
  (:documentation "Manifest options."))

(defun read-manifest (pathname)
  "Read a manifest file into an S-expression."
  (uiop:with-safe-io-syntax (:package (find-package :codex-manifest-user))
    (uiop:read-file-form pathname)))

(defun parse-manifest (pathname)
  (apply #'make-instance (cons 'manifest (read-manifest pathname))))

