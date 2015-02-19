(in-package :cl-user)
(defpackage codex.tmpl
  (:use :cl)
  (:documentation "Codex template definitions."))
(in-package :codex.tmpl)

(djula:add-template-directory
 (asdf:system-relative-pathname :codex #p"templates/"))

(defparameter +base+
  (djula:compile-template* "base.html"))

(defparameter +min-section+
  (djula:compile-template* "min/section.html"))
