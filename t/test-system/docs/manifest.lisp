(:docstring-markup-format :scriba
 :systems (:codex-test-system)
 :documents ((:title "Doc A"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :minima)
              :sources ("sec-a.scr"
                        "sec-b.scr"))
             (:title "Doc B"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :traditional)
              :sources ("sec-a.scr"
                        "sec-b.scr"))
             (:title "Doc C"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :gamma)
              :sources ("sec-a.scr"
                        "sec-b.scr"))))
