(in-package :codex)

(defparameter +manifest-template+
"(:docstring-markup-format :scriba
 :systems (:~A)
 :documents ((:title ~S
              :authors (~S)
              :output-format (:type :multi-html
                              :template :minima)
              :sources (\"manual.scr\"))))")

(defparameter +manual-template+
"@begin(section)
@title(Overview)

This is where you explain what your project does.

@end(section)

@begin(section)
@title(API Reference)

And this is where you list your project's functionality. Read the tutorial to
learn how to do this.

@end(section)
")

(defun clean-author-name (author)
  (string-trim '(#\Space)
               (ppcre:regex-replace "<.+>" author "")))

(defun quickstart (system-name)
  "Create a documentation folder, manifest, and a sample file for the given
system."
  (let* ((system (asdf:find-system system-name))
         (title (string-capitalize (asdf:component-name system)))
         (author (clean-author-name (asdf:system-author system)))
         (docs-directory (asdf:system-relative-pathname system-name #p"docs/"))
         (manifest-pathname (merge-pathnames #p"manifest.lisp"
                                             docs-directory))
         (manual-pathname (merge-pathnames #p"manual.scr"
                                           docs-directory)))
    (ensure-directories-exist docs-directory)
    (with-open-file (stream manifest-pathname
                            :direction :output
                            :if-exists :error
                            :if-does-not-exist :create)
      (format stream +manifest-template+
              (string-downcase (symbol-name system-name))
              title
              author))
    (with-open-file (stream manual-pathname
                            :direction :output
                            :if-exists :error
                            :if-does-not-exist :create)
      (write-string +manual-template+ stream))
    t))
