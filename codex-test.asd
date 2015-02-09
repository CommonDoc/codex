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
                ((:file "macros")
                 (:file "parser")
                 (:file "codex")))))
