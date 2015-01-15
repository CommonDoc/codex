(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam)
  (:import-from :codex-test.macros
                :macroexpansions))
(in-package :codex-test)

(run! 'macroexpansions)
