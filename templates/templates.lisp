(in-package :cl-user)
(defpackage codex.tmpl
  (:use :cl)
  (:import-from :common-html.template
   :template
   :render
  (:documentation "Codex template definitions."))
(in-package :codex.tmpl)

;;; Djula templates

(djula:add-template-directory
 (asdf:system-relative-pathname :codex #p"templates/"))

(defparameter +min-section+
  (djula:compile-template* "min/section.html"))

(defparameter +min-doc+
  (djula:compile-template* "min/document.html"))

