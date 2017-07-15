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
    :*log-format-time*
    :logm
    :*SIGINT*
    :handle-signal
    ))

(defpackage :snav
  (:use :cl :glu :split-sequence :uiop)
  (:documentation "Sole package of 'screen navigator'")
  (:export
    :run
    ))
