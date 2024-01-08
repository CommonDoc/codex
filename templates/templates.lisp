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
                     :documentation "The directory where the output will be produced.")
   (options :reader options
            :initarg :options
            :type property-list
            :documentation "A plist of template options."))
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
      ;; cl-fad:copy-file does almost the same thing, but cannot append.
      ;; This can be solved in SBCL with restarts, but we want this
      ;; code to be portable, don't we?
      (with-open-file (input-stream input-pathname
                                    :direction :input
                                    :element-type '(unsigned-byte 8))
        (with-open-file (output-stream output-pathname
                                       :direction :output
                                       :element-type '(unsigned-byte 8)
                                       ;; Important: The following allows us to
                                       ;; concatenate static files
                                       :if-exists :append
                                       :if-does-not-exist :create)
          (uiop:copy-stream-to-stream
           input-stream output-stream
           :element-type '(unsigned-byte 8)))))))

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

(defun append-mathjax-fonts (&rest files)
  (append
   (mapcar
    (lambda (pathname)
      (cons
       (uiop:enough-pathname pathname
                             (asdf:system-relative-pathname
                              :codex "templates/"))
       (make-pathname :name (pathname-name pathname)
                      :type (pathname-type pathname))))
    (cl-fad:list-directory
     (asdf:system-relative-pathname
      :codex "templates/static/mathjax/woff-v2/")))
   files))

(defun template-pathname (pathname)
  "Get a pathname of a file in templates directory."
  (merge-pathnames (pathname pathname) +templates-directory+))

(define-built-in-template minima
  :document-template (djula:compile-template*
                      (template-pathname #p"minima/document.html"))
  :section-template  (djula:compile-template*
                      (template-pathname #p"minima/section.html"))
  :static-files (append-mathjax-fonts
                 (cons #p"minima/style.css"
                       #p"style.css")
                 (cons #p"static/reset.css"
                       #p"style.css")
                 (cons #p"static/nodes.css"
                       #p"style.css")
                 (cons #p"static/highlight-lisp/highlight-lisp.js"
                       #p"highlight.js")
                 (cons #p"static/highlight-lisp/themes/github.css"
                       #p"highlight.css")
                 (cons #p"static/mathjax/tex-chtml.js"
                       #p"tex-chtml.js")
                 (cons #p"static/mathjax/load-mathjax.js"
                       #p"load-mathjax.js"))
  :documentation "Minimalist template.")

(define-built-in-template gamma
  :document-template (djula:compile-template*
                      (template-pathname #p"gamma/document.html"))
  :section-template  (djula:compile-template*
                      (template-pathname #p"gamma/section.html"))
  :static-files (append-mathjax-fonts
                 (cons #p"gamma/style.css"
                       #p"style.css")
                 (cons #p"static/reset.css"
                       #p"style.css")
                 (cons #p"static/nodes.css"
                       #p"style.css")
                 (cons #p"static/highlight-lisp/highlight-lisp.js"
                       #p"highlight.js")
                 (cons #p"static/highlight-lisp/themes/github.css"
                       #p"highlight.css")
                 (cons #p"static/mathjax/tex-chtml.js"
                       #p"tex-chtml.js")
                 (cons #p"static/mathjax/load-mathjax.js"
                       #p"load-mathjax.js"))
  :documentation "Modern template.")

;;; Template database

(defvar *template-database*
  (let ((table (make-hash-table)))
    (setf (gethash :minima table)
          (find-class 'minima))
    (setf (gethash :gamma table)
          (find-class 'gamma))
    table)
  "A hash table of table names (Keywords) to template classes.")

(defun find-template (template-name)
  "Find a template by name."
  (gethash template-name *template-database*))
