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
           :built-in-template
           :*template-database*
           :find-template)
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
   (static-files :reader template-static-files
                 :type (proper-list pathname)
                 :allocation :class
                 :documentation "The template's associated static files."))
  (:documentation "Codex templates."))

(defclass built-in-template (codex-template)
  ((document-template :reader document-template
                      :type string
                      :allocation :class
                      :documentation "The path to the document template.")
   (section-template :reader section-template
                     :type string
                     :allocation :class
                     :documentation "The path to the section template."))
  (:documentation "A convenience subclass for Codex built-in templates. These
  slots are not built into the default codex-template class, since users might
  define their own templates without using this machinery."))

(defmethod initialize-instance :after ((tmpl codex-template) &key)
  "Copy the template's CSS to the output directory"
  (ensure-directories-exist (merge-pathnames #p"static/"
                                             (template-directory tmpl)))
  (loop for (input . output) in (template-static-files tmpl) do
    (uiop:copy-file (merge-pathnames input
                                     (asdf:system-relative-pathname :codex
                                                                    #p"templates/"))
                    (merge-pathnames output
                                     (merge-pathnames #p"static/"
                                                      (template-directory tmpl))))))

(defmethod render ((tmpl built-in-template) (document document) content-string)
  "Render a built-in document template."
  (let ((template (djula:compile-template* (document-template tmpl)))
        (document-title (title document)))
    (djula:render-template* template
                            nil
                            :title document-title
                            :content content-string)))

(defmethod render-section ((tmpl built-in-template) (document document) (section section)
                           content-string)
  "Render a built-in section template."
  (let ((template (djula:compile-template* (section-template tmpl)))
        (document-title (title document))
        (section-title (common-doc.ops:collect-all-text (title section))))
    (djula:render-template* template
                            nil
                            :title document-title
                            :section-title section-title
                            :content content-string)))

;;; Built-in templates

(defclass min-template (built-in-template)
  ((document-template :initform "min/document.html")
   (section-template :initform "min/section.html")
   (static-files :initform (list
                            (cons #p"min/style.css"
                                  #p"style.css")
                            (cons #p"static/highlight-lisp/highlight-lisp.js"
                                  #p"highlight.js")
                            (cons #p"static/highlight-lisp/themes/github.css"
                                  #p"highlight.css"))))
  (:documentation "Minimalist template."))

;;; Template database

(defparameter *template-database*
  (let ((table (make-hash-table)))
    (setf (gethash :min table) (find-class 'min-template))
    table)
  "A hash table of table names (Keywords) to template classes.")

(defun find-template (template-name)
  "Find a template by name."
  (gethash template-name *template-database*))
