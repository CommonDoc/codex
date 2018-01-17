(defsystem codex-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Tests for Codex."
  :depends-on (:codex
               :vertex
               :cffi
               :common-html
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:module "manifests"
                  :components
                  ((:static-file "output.lisp")
                   (:static-file "template.lisp")))
                 (:module "test-system"
                  :components
                  ((:static-file "codex-test-system.asd")
                   (:static-file "test-system.lisp")
                   (:module "docs"
                    :components
                    ((:static-file "manifest.lisp")
                     (:static-file "sec-a.scr")
                     (:static-file "sec-b.scr")))))
                 (:file "codex")))))
