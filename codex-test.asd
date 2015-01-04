(defsystem codex-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:codex
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "macros")
                 (:file "codex")))))
