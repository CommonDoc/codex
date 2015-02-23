(in-package :codex.build)

(defun build-document (document directory)
  "Build a document."
  ;; Take all the document sources, concatenate them all, and parse them.
  (let* ((doc-source-files (loop for namestring in (document-sources document) collecting
                             (merge-pathnames (parse-namestring namestring)
                                              directory)))
         (sections (codex.markup:with-markup ((document-markup-format document))
                     (loop for file in doc-source-files collecting
                       (codex.markup:parse-string (uiop:read-file-string file)))))
         (doc (make-instance 'common-doc:document
                             :title (document-title document)
                             ;:authors (document-authors document)
                             :children sections))
         (build-directory (merge-pathnames #p"build/html/"
                                           directory))
         (output-format (document-output-format document))
         (html-template (codex.tmpl:find-template (html-template output-format))))
    ;; Expand macros
    (let ((doc (common-doc.macro:expand-macros doc)))
      (setf doc (common-doc.ops:fill-unique-refs doc))
      ;; Now we have a document, lets emit the HTML
      (if html-template
          (common-html.template:with-template (html-template :directory build-directory)
            (common-html.multi-emit:multi-emit doc
                                               build-directory
                                               :max-depth 3))
          (error 'codex.error:unknown-template
                 :template-name (html-template output-format)))
      doc)))

(defun build-manifest (manifest directory)
  "Build a manifest."
  ;; Populate the index with the documentation in the systems
  (let ((index (make-instance 'codex.index:index)))
    ;; Set the current markup format
    (codex.markup:with-markup ((markup-format manifest))
      (loop for system-name in (systems manifest) do
        (codex.parser:parse-system-into-index index system-name)))
    ;; Set the current index, before rendering documents
    (codex.index:with-index (index)
      ;; Now that we have the index, go through the list of documents, building
      ;; each one
      (loop for document in (documents manifest) collecting
        (build-document document directory)))))
