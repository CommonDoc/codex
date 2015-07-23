;;;; A manifest with an unsupported template
(:docstring-markup-format :scriba
 :systems (:codex-test-system)
 :documents ((:title "Test"
              :authors ("Test")
              :output-format (:type :multi-html
                              :template :no-such-template)
              :sources ("sec-a.scr"))))
