(in-package :snav)

(defun show-wmctrl-help ()
  (r-to-values (run-cmd "wmctrl --help") :strip-data? t))
