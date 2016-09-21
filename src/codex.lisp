(defpackage codex
  (:use :cl)
  (:import-from :common-html.template
                :with-template)
  (:import-from :common-html.multi-emit
                :multi-emit)
  (:import-from :codex.markup
                :*current-markup-format*)
  (:import-from :codex.manifest
                :document-title
                :document-sources
                :document-output-format
                :output-html-template
                :output-html-template-options)
  (:export :document
           :quickstart
           :*skip-undocumented*)
  (:documentation "The main interface."))
(in-package :codex)

(defvar *undocumented-list* nil
  "List of undocumented nodes")

(defvar *skip-undocumented* nil
  "If this variable is not NIL, do not call a debugger when undocumented node
is found.")

(defun load-document (document directory)
  "Load a CommonDoc document from the sources of a Codex document."
  ;; Take all the document sources, concatenate them all, and parse them.
  (let* ((sources (loop for namestring in (document-sources document)
                    collecting (merge-pathnames (parse-namestring namestring)
                                                directory)))
         ;; Set the markup format
         (*current-markup-format* (pandocl:guess-format (first sources)))
         ;; Parse the document
         (base-doc (codex.markup:parse-string
                    (apply #'concatenate
                           'string
                           (loop for file in sources collecting
                             (uiop:read-file-string file))))))
    (make-instance 'common-doc:document
                   :title (document-title document)
                   :children (common-doc:children base-doc))))

(defun copy-images (document directory)
  "Copy all the images from a CommonDoc document."
  (let ((images (common-doc.ops:collect-images document)))
    (loop for image in images do
      (handler-case
          (let* ((source (common-doc.file:absolute-path (common-doc:source image)))
                 (new-file (make-pathname :name (pathname-name source)
                                          :type (pathname-type source)
                                          :defaults directory)))
            (uiop:copy-file source new-file))
        (error ()
          ;; External image
          t)))))

(defun build-document (document directory)
  "Build a document."
  (let* ((doc (load-document document directory))
         (build-directory (make-pathname :directory (list :relative
                                                          (cl-slug:slugify (common-doc:title doc)))))
         (html-directory (merge-pathnames #p"html/" build-directory))
         (build-directory (merge-pathnames html-directory
                                           (merge-pathnames #p"build/"
                                                            directory)))
         (output-format (document-output-format document))
         (html-template (codex.tmpl:find-template
                         (output-html-template output-format)))
         ;; For document-external resources
         (common-doc.file:*base-directory* directory))
    ;; Expand macros
    (let ((doc (common-doc.macro:expand-macros doc)))
      ;; Delete the build directory
      (when (probe-file build-directory)
        (uiop:delete-directory-tree build-directory :validate t))
      ;; Ensure every section has a reference
      (setf doc (common-doc.ops:fill-unique-refs doc))
      ;; Now we have a document, lets emit the HTML
      (if html-template
          (with-template (html-template :output-directory build-directory
                                        :options (output-html-template-options output-format))
            (multi-emit doc build-directory :max-depth 1)
            (copy-images doc build-directory))
          (error 'codex.error:template-error
                 :template-name html-template
                 :message "No such template known."))
      doc)))

(defun undocumented-handler (c)
  "Handle undocumented nodes"
  (push (codex.error:node c)
        *undocumented-list*)
  (if (and *skip-undocumented*
           (find-restart 'codex.macro:use-docstring))
      (invoke-restart 'codex.macro:use-docstring "")))

(defun print-undocumented (undocumented)
  (flet ((print-node (node)
           (format t "No docstring for ~a (of type ~a)~%"
                   (docparser:node-name node)
                   (type-of node))))
    (mapc #'print-node undocumented)))

(defun build-manifest (manifest directory)
  "Build a manifest. Return a list of nodes which do not have a docstring."
  ;; First, load all the systems, extracting documentation information into the
  ;; global index
  (let ((codex.macro:*index* (docparser:parse (codex.manifest:manifest-systems manifest)))
        (*current-markup-format* (codex.manifest:manifest-markup-format manifest))
        *undocumented-list*)
    ;; Go through each document, building it
    (loop for document in (codex.manifest:manifest-documents manifest) do
         (handler-bind
             ((codex.error:no-docstring #'undocumented-handler))
           (build-document document directory)))
    *undocumented-list*))

(defun document (system-name &key (skip-undocumented *skip-undocumented*))
  "Generate documentation for a system."
  (let ((manifest-pathname (codex.manifest:system-manifest-pathname system-name)))
    (unless (probe-file manifest-pathname)
      (error 'codex.error:manifest-error
             :system-name system-name
             :message "No manifest."))
    (let ((manifest (codex.manifest:parse-manifest manifest-pathname))
          (directory (uiop:pathname-directory-pathname manifest-pathname))
          (*skip-undocumented* skip-undocumented))
      (print-undocumented
       (build-manifest manifest directory))))
  nil)
