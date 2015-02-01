(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam))
(in-package :codex-test)

(run! 'codex-test.macros:macroexpansions)
(run! 'codex-test.parser:parser)
