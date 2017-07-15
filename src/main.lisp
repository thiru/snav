(in-package :snav)

(defun show-wmctrl-help ()
  (r-to-values (run-cmd "wmctrl --help") :strip-data? t))

(defun go-to-workspace (num)
  "Go to workspace number `num` (1-based index)."
  (r-to-values (run-cmd (sf "wmctrl -s ~A" (max 0 (1- (or num 1)))))))
