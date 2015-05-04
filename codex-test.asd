(defsystem codex-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Tests for Codex."
  :depends-on (:codex
               :vertex
               :common-html
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:module "test-system"
                  :components
                  ((:static-file "codex-test-system.asd")
                   (:static-file "test-system.lisp")
                   (:module "docs"
                    :components
                    ((:static-file "manifest.lisp")
                     (:static-file "doc-a.scr")
                     (:static-file "doc-b.tex")))))
                 (:file "codex")))))
