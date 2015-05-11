(defsystem codex-templates
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-html
               :djula
               :trivial-types)
  :components ((:module "templates"
                :serial t
                :components
                ((:static-file "base.html")
                 (:static-file "document.html")
                 (:static-file "section.html")
                 (:file "templates")
                 (:module "static"
                  :components
                  ((:static-file "nodes.css")
                   (:static-file "reset.css")))
                 (:module "min"
                  :components
                  ((:static-file "style.css")))
                 (:module "traditional"
                  :components
                  ((:static-file "style.css"))))))
  :description "Templates for Codex.")
