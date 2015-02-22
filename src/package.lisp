(in-package :cl-user)
(defpackage codex.error
  (:use :cl)
  (:export :codex-error
           :no-manifest
           :unsupported-markup-format
           :system-name
           :format-name)
  (:documentation "Codex errors."))

(defpackage codex.markup
  (:use :cl)
  (:export :with-markup
           :parse-string)
  (:documentation "Parsing files and docstrings."))

(defpackage codex.macro
  (:use :cl :trivial-types)
  (:import-from :common-doc
                :define-node
                :text-node
                :content-node
                :document-link
                :children
                :text)
  (:import-from :common-doc.macro
                :expand-macro)
  (:import-from :common-doc.util
                :make-meta
                :make-text)
  (:export :*current-package*
           :cl-ref
           :symbol-node
           :function-node
           :macro-node
           :generic-function-node
           :method-node
           :variable-node
           :record-node
           :slot-node
           :struct-node
           :class-node
           :type-node
           :symbol-node-package
           :symbol-node-name
           :externalp
           :setfp
           :render-full-symbol
           :doc-symbol
           :doc-description
           :operator-lambda-list
           :record-slots
           :slot-accessors
           :slot-readers
           :slot-writers)
  (:documentation "CommonDoc macros for representing documentation."))

(defpackage codex.index
  (:use :cl)
  (:export :index
           :add-node
           :get-node
           :*current-index*
           :with-index
           :get-from-current-index)
  (:documentation "An index is a data structure that associates symbol names to
  their CommonDoc nodes so they can be included in documentation output."))

(defpackage codex.parser
  (:use :cl :trivial-types)
  (:import-from :codex.index
                :add-node)
  (:export :parse-variable
           :parse-operator
           :parse-record
           :parse-system-into-index)
  (:documentation "Given a system name, create a CommonDoc document from the
  documentation."))

(defpackage codex.manifest
  (:use :cl :trivial-types)
  (:export :*default-manifest-pathname*
           :system-manifest-pathname
           :document
           :document-title
           :document-authors
           :document-sources
           :manifest
           :markup-format
           :systems
           :documents
           :parse-manifest)
  (:documentation "Parsing Codex manifest files."))

(defpackage :codex-manifest-user
  (:use :cl :codex.manifest)
  (:documentation "The package in which Codex manifests are read."))

(defpackage codex.build
  (:use :cl)
  (:import-from :codex.manifest
                :document
                :document-title
                :document-authors
                :document-sources
                :manifest
                :markup-format
                :systems
                :documents)
  (:export :build-document
           :build-manifest)
  (:documentation "Build manifests and documents."))

(defpackage codex
  (:use :cl)
  (:export :document)
  (:documentation "The main interface."))
