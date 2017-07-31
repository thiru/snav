(in-package :snav)

;;; Structs --------------------------------------------------------------------

(defstruct app-info
  (name "snav")
  (debug-mode? nil)
  (app-dir "")
  (version "")
  (last-updated "")
  (tmp-file-path ""))

(defstruct workspace
  (num 0 :type integer)
  (active? nil)
  (name "" :type string))

;;; Structs ====================================================================

;;; Init -----------------------------------------------------------------------

(defun create-app-info ()
  "Initialise the app."
  (let* ((app-dir (asdf:system-source-directory :snav))
         (version-file-path (asdf:system-relative-pathname :snav "version")))
    (make-app-info :debug-mode? nil
                   :app-dir app-dir 
                   :version (asdf::read-file-form version-file-path)
                   :last-updated
                   (universal-to-timestamp
                     (file-write-date version-file-path))
                   :tmp-file-path
                   (merge-pathnames* "snav-last-active-workspace.txt"
                                     (temporary-directory)))))

(defvar *app-info* (create-app-info))

;;; Init =======================================================================

;;; Helper Functions -----------------------------------------------------------

(defun as-safe-workspace-num (num)
  (max 1 (or num 1)))

(defun as-safe-wmctrl-workspace-num (num)
  (max 0 (or num 1)))

(defun get-active-workspace-num ()
  (let* ((cmd-r (run-cmd "xprop -root -notype _NET_CURRENT_DESKTOP")))
    (if (succeeded? cmd-r)
      (as-safe-workspace-num
        (1+ (loose-parse-int (last1 (split-string (r-data cmd-r)))))))))

(defun get-workspace-count ()
  "Get the total number of workspaces."
  (let* ((cmd-r (run-cmd "xprop -root -notype _NET_NUMBER_OF_DESKTOPS")))
    (if (succeeded? cmd-r)
      (loose-parse-int (last1 (split-string (r-data cmd-r)))))))

(defun parse-workspaces ()
  "Create `workspace` structs from `wmctrl -d` command."
  (let* ((list-workspaces-cmd-r (run-cmd "wmctrl -d"))
         (workspace-lines '())
         (workspaces '())
         (curr-workspace-num 0)
         (curr-workspace-active? nil))
    (if (succeeded? list-workspaces-cmd-r)
      (progn
        (setf workspace-lines
              (split-sequence #\linefeed (r-data list-workspaces-cmd-r)))
        (dolist (line workspace-lines)
          (setf curr-workspace-num
                (1+ (loose-parse-int (cl-ppcre:scan-to-strings "^\\d+" line))))
          (setf curr-workspace-active?
                (string-equal " * "
                              (cl-ppcre:scan-to-strings " [\\-\\*] " line)))
          (push (make-workspace :num curr-workspace-num
                                :active? curr-workspace-active?
                                :name "todo")
                workspaces))))
    (nreverse workspaces)))

(defun save-active-workspace-num (&optional num)
  "Save the active workspace, `num` to a temporary file. If `num` is not given
   the current workspace is used."
  (with-open-file (stream
                    (app-info-tmp-file-path *app-info*)
                    :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create)
    (write (as-safe-workspace-num num) :stream stream :readably t)))

(defun get-last-active-workspace-num ()
  "Get the last active workspace number. If none is found default to 1."
  ;; TODO: catch I/O error and return 1 as fallback
  (if (file-exists-p (app-info-tmp-file-path *app-info*))
    (asdf::read-file-form (app-info-tmp-file-path *app-info*))
    1))

;;; Helper Functions ===========================================================

;;; Public Functions -----------------------------------------------------------

(defun go-to-workspace (num)
  "Go to workspace number `num` (1-based index)."
  (save-active-workspace-num (get-active-workspace-num))
  (r-to-values (run-cmd (sf "wmctrl -s ~A" (1- (as-safe-wmctrl-workspace-num num))))))

(defun go-to-next-workspace ()
  "Go to the next ordinal workspace."
  (let* ((active-workspace-num (get-active-workspace-num))
         (num-workspaces (get-workspace-count)))
    (if (>= active-workspace-num num-workspaces)
      (go-to-workspace 1)
      (go-to-workspace (1+ active-workspace-num)))))

(defun go-to-previous-workspace ()
  "Go to the previous ordinal workspace."
  (let* ((active-workspace-num (get-active-workspace-num)))
    (if (= 1 active-workspace-num)
      (go-to-workspace (get-workspace-count))
      (go-to-workspace (1- active-workspace-num)))))

(defun go-to-last-active-workspace ()
  "Go to the workspace that was active last (like cycling to the last active
   window using ALT-TAB)."
  (go-to-workspace (get-last-active-workspace-num)))

;;; Public Functions ===========================================================
