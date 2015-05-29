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

;;; Constants

(defparameter +templates-directory+
  (asdf:system-relative-pathname :codex #p"templates/"))

;;; Djula templates

(djula:add-template-directory +templates-directory+)

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
                      :initform "document.html"
                      :type string
                      :allocation :class
                      :documentation "The path to the document template.")
   (section-template :reader section-template
                     :initform "section.html"
                     :type string
                     :allocation :class
                     :documentation "The path to the section template."))
  (:documentation "A convenience subclass for Codex built-in templates. These
  slots are not built into the default codex-template class, since users might
  define their own templates without using this machinery."))

(defun static-output-directory (built-in-template)
  (merge-pathnames #p"static/"
                   (template-directory built-in-template)))

(defmethod initialize-instance :after ((tmpl codex-template) &key)
  "Copy the template's static files to the output directory"
  (ensure-directories-exist (static-output-directory tmpl))
  (loop for (input . output) in (template-static-files tmpl) do
    (let ((input-pathname (merge-pathnames input
                                           +templates-directory+))
          (output-pathname (merge-pathnames output
                                            (static-output-directory tmpl))))
      (with-open-file (input-stream input-pathname
                                    :direction :input)
        (with-open-file (output-stream output-pathname
                                       :direction :output
                                       ;; Important: The following allows us to
                                       ;; concatenate static files
                                       :if-exists :append
                                       :if-does-not-exist :create)
          (uiop:copy-stream-to-stream input-stream output-stream))))))

(defmethod render ((tmpl built-in-template) (document document) content-string)
  "Render a built-in document template."
  (let ((template (djula:compile-template* (document-template tmpl)))
        (document-title (title document)))
    (djula:render-template* template
                            nil
                            :document-title document-title
                            :content content-string)))

(defmethod render-section ((tmpl built-in-template) (document document) (section section)
                           content-string)
  "Render a built-in section template."
  (let ((template (djula:compile-template* (section-template tmpl)))
        (document-title (title document))
        (section-title (common-doc.ops:collect-all-text (title section)))
        (toc (common-html.toc:multi-file-toc document :max-depth 1)))
    (djula:render-template* template
                            nil
                            :document-title document-title
                            :section-title section-title
                            :toc toc
                            :content content-string)))

;;; Built-in templates

(defclass minima (built-in-template)
  ((static-files :initform (list
                            (cons #p"minima/style.css"
                                  #p"style.css")
                            (cons #p"static/reset.css"
                                  #p"style.css")
                            (cons #p"static/nodes.css"
                                  #p"style.css")
                            (cons #p"static/highlight-lisp/highlight-lisp.js"
                                  #p"highlight.js")
                            (cons #p"static/highlight-lisp/themes/github.css"
                                  #p"highlight.css"))))
  (:documentation "Minimalist template."))

(defclass traditional-template (built-in-template)
  ((static-files :initform (list
                            (cons #p"traditional/style.css"
                                  #p"style.css")
                            (cons #p"static/reset.css"
                                  #p"style.css")
                            (cons #p"static/nodes.css"
                                  #p"style.css")
                            (cons #p"static/highlight-lisp/highlight-lisp.js"
                                  #p"highlight.js")
                            (cons #p"static/highlight-lisp/themes/github.css"
                                  #p"highlight.css"))))
  (:documentation "Traditional template."))

;;; Template database

(defvar *template-database*
  (let ((table (make-hash-table)))
    (setf (gethash :minima table) (find-class 'minima))
    (setf (gethash :traditional table)
          (find-class 'traditional-template))
    table)
  "A hash table of table names (Keywords) to template classes.")

(defun find-template (template-name)
  "Find a template by name."
  (gethash template-name *template-database*))
