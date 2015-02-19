(defsystem codex-templates
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-html
               :djula)
  :components ((:module "templates"
                :serial t
                :components
                ((:static-file "base.html")
                 (:module "min"
                  :components
                  ((:static-file "section.html")))
                 (:file "templates"))))
  :description "Templates for Codex.")
