(in-package :cl-user)
(defpackage codex.tmpl
  (:use :cl)
  (:import-from :common-html.template
                :template
                :render
                :render-section)
  (:documentation "Codex template definitions."))
(in-package :codex.tmpl)

;;; Djula templates

(djula:add-template-directory
 (asdf:system-relative-pathname :codex #p"templates/"))

(defparameter +min-section+
  (djula:compile-template* "min/section.html"))

(defparameter +min-doc+
  (djula:compile-template* "min/document.html"))

;;; Code

(defclass codex-template (template)
  ((directory :reader template-directory
              :initarg :directory
              :type pathname
              :documentation "The directory where the output will be produced.")
   (css-file :reader template-css-file
             :initarg :css-file
             :type pathname
             :documentation "The CSS file."))
  (:documentation "Codex templates."))

(defmethod initialize-instance :after ((tmpl codex-template) &key)
  "Copy the template's CSS to the output directory"
  (ensure-directories-exist (merge-pathnames #p"static/"
                                             (template-directory tmpl)))
  (uiop:copy-file (merge-pathnames (template-css-file tmpl)
                                   (asdf:system-relative-pathname :codex
                                                                  #p"templates/"))
                  (merge-pathnames #p"static/style.css"
                                   (template-directory tmpl))))

;; Signatures:
;; render ((template template) (document document) content-string
;; render-section ((template template) (document document) (section section) content-string
