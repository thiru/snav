;;;; Build configuration

(in-package :cl-user)

(asdf:defsystem :snav
  :version (:read-file-form "version")
  :description "A window and workspace navigator for X"
  :author "Thirushanth Thirunavukarasu <thiru0130@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (:alexandria :cffi :cl-ppcre :local-time :split-sequence :uiop)
  :components ((:file "src/packages")
               (:file "src/glu")
               (:file "src/main")
               ))
