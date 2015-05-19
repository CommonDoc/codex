(:docstring-markup-format :scriba
 :systems (:codex-test-system)
 :documents ((:title "Doc A"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :min)
              :sources ("doc-a.scr"))
             (:title "Doc B"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :traditional)
              :sources ("doc-a.scr"
                        "doc-b.scr"))))
