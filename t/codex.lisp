(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam)
  (:export :do-tests))
(in-package :codex-test)

(defun do-tests ()
  (run! 'tests))

;;; Constants

(defparameter +doc-build-directory+
  (asdf:system-relative-pathname :codex-test-system
                                 #p"docs/build/"))

;;; Tests

(def-suite tests)
(in-suite tests)

(test manifest-error
  (signals codex.error:unsupported-output-format
    (codex.manifest:parse-manifest
     (asdf:system-relative-pathname :codex-test
                                    #p"t/manifests/output.lisp")))
  (signals codex.error:template-error
    (let* ((manifest-pathname #p"t/manifests/template.lisp")
           (manifest (codex.manifest:parse-manifest
                      (asdf:system-relative-pathname :codex-test
                                                     manifest-pathname)))
           (build-directory (asdf:system-relative-pathname :codex-test-system
                                                           #p"docs/")))
      (codex::build-manifest manifest build-directory)))
  (signals codex.error:manifest-error
    ;; Try to document a system that certainly has no manifest
    (codex:document :alexandria)))

(test set-up
  ;; Ensure the test system's docs build directory is empty
  (finishes
    (when (probe-file +doc-build-directory+)
      (uiop:delete-directory-tree +doc-build-directory+
                                  :validate t))))

(test document-test-system
  (finishes
   (codex:document :codex-test-system)))

(test files-exist
  (let ((files (list #p"doc-a/html/section-a.html"
                     #p"doc-a/html/static/style.css"
                     #p"doc-a/html/static/highlight.js"
                     #p"doc-a/html/static/highlight.css"
                     #p"doc-a/html/test.png")))
    (loop for file in files do
      (is-true
       (probe-file (merge-pathnames file
                                    +doc-build-directory+))))))

(test macros-work
  ;; Ensure all the names are right
  (is-false
   (search "No node with name"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-a/html/section-a.html"
                              +doc-build-directory+))))
  ;; Ensure all nodes are supported
  (is-false
   (search "Unsupported node type"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-a/html/section-a.html"
                              +doc-build-directory+))))
  (is-true
   (search "http://l1sp.org/cl/defpackage"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-a/html/section-a.html"
                              +doc-build-directory+))))
  (is-false
   (search "No node type with name"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-b/html/section-a.html"
                              +doc-build-directory+))))
  (is-true
   (search "No node type with name"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-b/html/section-b.html"
                              +doc-build-directory+))))
  (is-true
   (search "No node with name"
           (uiop:read-file-string
            (merge-pathnames  #p"doc-b/html/section-b.html"
                              +doc-build-directory+)))))
