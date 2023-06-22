(defsystem codex-templates
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-html
               :djula
               :cl-fad
               :trivial-types)
  :components ((:module "templates"
                :serial t
                :components
                ((:file "templates")
                 (:module "static"
                  :components
                  ((:static-file "nodes.css")
                   (:static-file "reset.css")))
                 (:module "minima"
                  :components
                  ((:static-file "base.html")
                   (:static-file "document.html")
                   (:static-file "section.html")
                   (:static-file "style.css")))
                 (:module "gamma"
                  :components
                  ((:static-file "base.html")
                   (:static-file "document.html")
                   (:static-file "section.html")
                   (:static-file "style.css")))
                 (:module "traditional"
                  :components
                  ((:static-file "base.html")
                   (:static-file "document.html")
                   (:static-file "section.html")
                   (:static-file "style.css"))))))
  :description "Templates for Codex.")
