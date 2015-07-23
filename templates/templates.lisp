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

(defparameter +templates-directory+
  (asdf:system-relative-pathname :codex #p"templates/"))

;;; Classes

(defclass codex-template (template)
  ((output-directory :reader output-directory
                     :initarg :output-directory
                     :type pathname
                     :documentation "The directory where the output will be produced."))
  (:documentation "Codex templates."))

(defclass built-in-template (codex-template)
  ()
  (:documentation "A convenience subclass for Codex built-in templates. These
  slots are not built into the default codex-template class, since users might
  define their own templates without using this machinery."))

(defgeneric document-template (template))

(defgeneric section-template (template))

(defgeneric static-files (template))

;;; Methods

(defun static-output-directory (built-in-template)
  (merge-pathnames #p"static/"
                   (output-directory built-in-template)))

(defmethod initialize-instance :after ((tmpl codex-template) &key)
  "Copy the template's static files to the output directory"
  (ensure-directories-exist (static-output-directory tmpl))
  (loop for (input . output) in (static-files tmpl) do
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
  (let ((template (document-template tmpl))
        (document-title (title document)))
    (djula:render-template* template
                            nil
                            :document-title document-title
                            :content content-string)))

(defmethod render-section ((tmpl built-in-template) (document document) (section section)
                           content-string)
  "Render a built-in section template."
  (let ((template (section-template tmpl))
        (document-title (title document))
        (section-title (common-doc.ops:collect-all-text (title section)))
        (section-ref (common-doc:reference section))
        (toc (common-html.toc:multi-file-toc document)))
    (djula:render-template* template
                            nil
                            :document-title document-title
                            :section-title section-title
                            :section-ref section-ref
                            :toc toc
                            :content content-string)))

;;; Built-in templates

(djula:add-template-directory +templates-directory+)

(defmacro define-built-in-template (name &key document-template section-template
                                           static-files
                                           documentation)
  `(progn
     (defclass ,name (built-in-template)
       ()
       (:documentation ,documentation))

     (defmethod document-template ((template ,name))
       ,document-template)

     (defmethod section-template ((template ,name))
       ,section-template)

     (defmethod static-files ((template ,name))
       ,static-files)))

(define-built-in-template minima
  :document-template (djula:compile-template* "minima/document.html")
  :section-template (djula:compile-template* "minima/section.html")
  :static-files (list
                 (cons #p"minima/style.css"
                       #p"style.css")
                 (cons #p"static/reset.css"
                       #p"style.css")
                 (cons #p"static/nodes.css"
                       #p"style.css")
                 (cons #p"static/highlight-lisp/highlight-lisp.js"
                       #p"highlight.js")
                 (cons #p"static/highlight-lisp/themes/github.css"
                       #p"highlight.css"))
  :documentation "Minimalist template.")

(define-built-in-template gamma
  :document-template (djula:compile-template* "gamma/document.html")
  :section-template (djula:compile-template* "gamma/section.html")
  :static-files (list
                 (cons #p"gamma/style.css"
                       #p"style.css")
                 (cons #p"static/reset.css"
                       #p"style.css")
                 (cons #p"static/nodes.css"
                       #p"style.css")
                 (cons #p"static/highlight-lisp/highlight-lisp.js"
                       #p"highlight.js")
                 (cons #p"static/highlight-lisp/themes/github.css"
                       #p"highlight.css"))
  :documentation "Modern template.")

(define-built-in-template traditional
  :document-template (djula:compile-template* "traditional/document.html")
  :section-template (djula:compile-template* "traditional/section.html")
  :static-files (list
                 (cons #p"traditional/style.css"
                       #p"style.css")
                 (cons #p"static/reset.css"
                       #p"style.css")
                 (cons #p"static/nodes.css"
                       #p"style.css")
                 (cons #p"static/highlight-lisp/highlight-lisp.js"
                       #p"highlight.js")
                 (cons #p"static/highlight-lisp/themes/github.css"
                       #p"highlight.css"))
  :documentation "Traditional template.")

;;; Template database

(defvar *template-database*
  (let ((table (make-hash-table)))
    (setf (gethash :minima table)
          (find-class 'minima))
    (setf (gethash :gamma table)
          (find-class 'gamma))
    (setf (gethash :traditional table)
          (find-class 'traditional))
    table)
  "A hash table of table names (Keywords) to template classes.")

(defun find-template (template-name)
  "Find a template by name."
  (gethash template-name *template-database*))
