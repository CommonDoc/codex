(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam)
  (:import-from :codex-test.macros
                :tests))
(in-package :codex-test)

(run! 'tests)
