(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam))
(in-package :codex-test)

;;; Constants

(defparameter +doc-build-directory+
  (asdf:system-relative-pathname :codex-test-system
                                 #p"docs/build/"))

;;; Tests

(def-suite tests)
(in-suite tests)

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
  (let ((files (list #p"html/section-a.html"
                     #p"html/static/style.css"
                     #p"html/static/highlight.js"
                     #p"html/static/highlight.css")))
    (loop for file in files do
      (is-true
       (probe-file (merge-pathnames file
                                    +doc-build-directory+))))))

(test macros-work
  ;; Ensure all the names are right
  (is-false
   (search "No node with name"
           (uiop:read-file-string
            (merge-pathnames  #p"html/section-a.html"
                              +doc-build-directory+))))
  ;; Ensure all nodes are supported
  (is-false
   (search "Unsupported node type"
           (uiop:read-file-string
            (merge-pathnames  #p"html/section-a.html"
                              +doc-build-directory+)))))

(run! 'tests)
