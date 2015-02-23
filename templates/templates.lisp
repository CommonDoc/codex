(in-package :cl-user)
(defpackage codex.tmpl
  (:use :cl :trivial-types)
  (:import-from :common-doc
                :document
                :section
                :title)
  (:import-from :common-html.template
                :template
                :render
                :render-section)
  (:export :codex-template
           :built-template)
  (:documentation "Codex template definitions."))
(in-package :codex.tmpl)

;;; Djula templates

(djula:add-template-directory
 (asdf:system-relative-pathname :codex #p"templates/"))

;;; Code

(defclass codex-template (template)
  ((directory :reader template-directory
              :initarg :directory
              :type pathname
              :documentation "The directory where the output will be produced.")
   (css-files :reader template-css-files
              :type (proper-list pathname)
              :allocation :class
              :documentation "The CSS files."))
  (:documentation "Codex templates."))

(defclass built-in-template (codex-template)
  ((document-template :reader document-template
                      :type pathname
                      :allocation :class
                      :documentation "The path to the document template.")
   (section-template :reader section-template
                     :type pathname
                     :allocation :class
                     :documentation "The path to the section template."))
  (:documentation "A convenience subclass for Codex built-in templates. These
  slots are not built into the default codex-template class, since users might
  define their own templates without using this machinery."))

(defmethod initialize-instance :after ((tmpl codex-template) &key)
  "Copy the template's CSS to the output directory"
  (ensure-directories-exist (merge-pathnames #p"static/"
                                             (template-directory tmpl)))
  (loop for file in (template-css-file tmpl) do
    (uiop:copy-file (merge-pathnames file
                                     (asdf:system-relative-pathname :codex
                                                                    #p"templates/"))
                    (merge-pathnames #p"static/style.css"
                                     (template-directory tmpl)))))

(defmethod render ((tmpl built-in-template) (document document) content-string)
  "Render a built-in document template."
  (let ((template (djula:compile-template* (document-template tmpl)))
        (document-title (title document)))
    (djula:render-template* template nil
                            :title document-title
                            :content content-string)))

(defmethod render-section ((tmpl built-in-template) (document document) (section section)
                           content-string)
  "Render a built-in section template."
  (let ((template (djula:compile-template* (section-template tmpl)))
        (document-title (title document))
        (section-title (common-doc.ops:collect-all-text (title section))))
    (djula:render-template* template nil
                            :title document-title
                            :section-title section-title
                            :content content-string)))

;;; Built-in templates

(defclass min-template (built-in-template)
  ((document-template :initform #p"min/document.html")
   (section-template :initform #p"min/section.html")
   (css-files :initform (list #p"min/style.css")))
  (:documentation "Minimalist template."))
