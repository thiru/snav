;;;; Package definitions

(in-package :cl-user)

(defpackage :glu
  (:use :cl :local-time)
  (:documentation "Sole package of 'Global Lisp Utilities'")
  (:export
    :*english-list*
    :1st
    :2nd
    :last1
    :blank?
    :join
    :trim
    :->
    :=>
    :loose-parse-int
    :sf
    :to-string
    :labeled-time
    :get-run-time
    :display-run-time
    :pretty-time
    :empty/*objects*
    :empty
    :empty=>
    :empty?
    :levels
    :r
    :r-level
    :r-message
    :r-data
    :new-r
    :succeeded?
    :failed?
    :r-to-values
    :*log-format-time*
    :logm
    :*SIGINT*
    :handle-signal
    :run-cmd
    ))

(defpackage :snav
  (:use :cl :glu :local-time :split-sequence :uiop)
  (:documentation "Sole package of 'screen navigator'")
  (:export
    :app-info
    :app-info-name
    :app-info-debug-mode?
    :app-info-app-dir
    :app-info-version
    :app-info-last-updated
    :*app-info*
    :go-to-workspace
    :go-to-next-workspace
    :go-to-previous-workspace
    :go-to-last-active-workspace
    ))
