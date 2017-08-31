;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :snav
  :version (:read-file-form "version")
  :description "A window, workspace and monitor navigator for X"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :cffi :cl-annot :cl-ppcre :local-time :split-sequence :uiop)
  :components ((:file "src/glu/core")
               (:file "src/glu/map")
               (:file "src/glu/empty")
               (:file "src/glu/validation")
               (:file "src/glu/terminal")
               (:file "src/main")
               ))
