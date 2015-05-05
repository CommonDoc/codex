(in-package :cl-user)
(defpackage codex-test
  (:use :cl :fiveam))
(in-package :codex-test)

(def-suite tests)
(in-suite tests)

(test document-test-system
  (finishes
   (codex:document :codex-test-system)))

(run! 'tests)
