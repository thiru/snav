(in-package :snav)

(defstruct workspace
  (num 0 :type integer)
  (active? nil)
  (name "" :type string))

(defun show-wmctrl-help ()
  (r-to-values (run-cmd "wmctrl --help") :strip-data? t))

(defun go-to-workspace (num)
  "Go to workspace number `num` (1-based index)."
  (r-to-values (run-cmd (sf "wmctrl -s ~A" (max 0 (1- (or num 1)))))))

(defun go-to-next-workspace ()
  "Go to the next ordinal workspace."
  (let* ((workspaces (parse-workspaces)))
    (dolist (workspace workspaces)
      (if (workspace-active? workspace)
        (if (>= (workspace-num workspace) (length workspaces))
          (go-to-workspace 1)
          (go-to-workspace (1+ (workspace-num workspace))))))))

(defun parse-workspaces ()
  "Create `workspace` structs from `wmctrl -d` command."
  (let* ((list-workspaces-cmd-r (run-cmd "wmctrl -d"))
         (workspace-lines '())
         (workspaces '())
         (curr-workspace-num 0)
         (curr-workspace-active? nil))
    (if (succeeded? list-workspaces-cmd-r)
      (progn
        (setf workspace-lines (split-sequence #\linefeed (r-data list-workspaces-cmd-r)))
        (dolist (line workspace-lines)
          (setf curr-workspace-num (1+ (loose-parse-int (cl-ppcre:scan-to-strings "^\\d+" line))))
          (setf curr-workspace-active? (string-equal " * " (cl-ppcre:scan-to-strings " [\\-\\*] " line)))
          (push (make-workspace :num curr-workspace-num
                                :active? curr-workspace-active?
                                :name "todo")
                workspaces))))
    (nreverse workspaces)))
