(in-package :cl-user)
(defpackage codex.manifest
  (:use :cl :trivial-types)
  (:documentation "Parsing Codex manifest files."))
(in-package :codex.manifest)

(defun system-manifest-pathname (system-name)
  "Return the path to a system's manifest file."
  (merge-pathnames #p"codex.lisp-expr"
                   (asdf:component-pathname
                    (asdf:find-system system-name))))

(defun read-manifest (pathname)
  "Read a manifest file into an S-expression."
  (with-open-file (input-stream pathname
                                :direction :input)
    (read input-stream)))

(defclass <config> ()
  ((project-name :reader project-name
                 :initarg project-name
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
  (:documentation "Documentation options."))
