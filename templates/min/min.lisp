(in-package :cl-user)
(defpackage codex.tmpl.min
  (:use :cl)
  (:import-from :common-html.template
                :render
                :render-section)
  (:import-from :codex.tmpl
                :codex-template)
  (:export :min-template)
  (:documentation "Codex template definitions."))
(in-package :codex.tmpl)

(defclass min-template (codex-template)
  ((css-files :initform (list #p"min/style.css")
              :type (proper-list pathname)
              :allocation :class))
  (:documentation "Minimalist template."))
