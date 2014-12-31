(in-package :cl-user)
(defpackage codex.parser
  (:use :cl :trivial-types)
  (:documentation "Given a system name, create a CommonDoc document from the
  documentation."))
(in-package :codex.parser)

(defclass <system> ()
  ((name :reader system-name
         :initarg :name
         :type string
         :documentation "The nameo of the system.")
   (author :reader system-author
           :initarg :author
           :type string
           :documentation "The name of the author.")
   (maintainer :reader system-maintainer
               :initarg :maintainer
               :type string
               :documentation "The system's current maintainer.")
   (license :reader system-license
            :initarg :license
            :type string
            :documentation "The system's license.")
   (dependencies :reader system-dependencies
                 :initarg :dependencies
                 :type (proper-list string)
                 :documentation "A list of systems this system depends on.")
   (packages :reader system-packages
             :initarg :packages
             :type list
             :documentation "A list of the system's packages."))
  (:documentation "A parsed ASDF system."))

(defun parse-system (system-name)
  (let ((parsed (quickdocs.parser:parse-documentation system-name)))
    (make-instance '<system>
                   :name (getf parsed :name)
                   :author (getf parsed :author)
                   :maintainer (getf parsed :maintainer)
                   :license (getf parsed :licence)
                   :dependencies (getf parsed :depends-on)
                   :packages (getf parsed :package-list))))
