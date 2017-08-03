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

(defstruct window
  (id "" :type string)
  (pid 0 :type integer)
  (name "" :type string) 
  (workspace-num -1 :type integer)
  (x-offset 0 :type integer)
  (y-offset 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  (pc-name "" :type string))

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

;; NOTE: Not currently being used
(defun parse-workspaces ()
  "Create `workspace` structs from `wmctrl -d` command."
  (let* ((list-workspaces-cmd-r (run-cmd "wmctrl -d"))
         (cmd-output-lines '())
         (workspaces '())
         (curr-workspace-num 0)
         (curr-workspace-active? nil)
         (curr-workspace-name ""))
    (if (succeeded? list-workspaces-cmd-r)
      (progn
        (setf cmd-output-lines
              (split-sequence #\linefeed (r-data list-workspaces-cmd-r)))
        (dolist (line cmd-output-lines)
          (setf curr-workspace-num
                (1+ (loose-parse-int (cl-ppcre:scan-to-strings "^\\d+" line))))
          (setf curr-workspace-active?
                (string-equal " * "
                              (cl-ppcre:scan-to-strings " [\\-\\*] " line)))
          (setf curr-workspace-name
                (multiple-value-bind
                  (whole-match sub-matches)
                  (cl-ppcre:scan-to-strings "^.+\\d+x\\d+  (.+)$" line)
                  (declare (ignore whole-match))
                  (if (plusp (length sub-matches))
                    (aref sub-matches 0)
                    "")))
          (push (make-workspace :num curr-workspace-num
                                :active? curr-workspace-active?
                                :name curr-workspace-name)
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

(defun list-windows (&key workspace)
  "Create `window` structs from `wmctrl -l -p -G` command. The `-l` option
   specifies to list window information. The `-p` option specifies to show
   the PID of the window. The `-G` option specifies to show window geometry
   (x/y offsets, width, height).
   Stick windows and windows, windows with no size and the desktop pseudo
   window are excluded.
   If `workspace` is specified, only windows belonging to the respective
   workspace is returned."
  (let* ((list-windows-cmd-r (run-cmd "wmctrl -l -p -G"))
         (cmd-output-lines '())
         (curr-line-segs '())
         (curr-window nil)
         (windows '()))
    (if (succeeded? list-windows-cmd-r)
      (progn
        (setf cmd-output-lines
              (split-sequence #\linefeed (r-data list-windows-cmd-r)))
        (dolist (line cmd-output-lines)
          (setf curr-line-segs (cl-ppcre:split "\\s+" line))
          (when (<= 9 (length curr-line-segs))
            (setf curr-window
                  (make-window :id (nth 0 curr-line-segs)
                               :workspace-num
                               (1+ (loose-parse-int (nth 1 curr-line-segs)))
                               :pid (loose-parse-int (nth 2 curr-line-segs))
                               :x-offset (loose-parse-int (nth 3 curr-line-segs))
                               :y-offset (loose-parse-int (nth 4 curr-line-segs))
                               :width (loose-parse-int (nth 5 curr-line-segs))
                               :height (loose-parse-int (nth 6 curr-line-segs))
                               :pc-name (nth 7 curr-line-segs)
                               :name
                               (format nil
                                       "~{~A~^ ~}"
                                       (subseq curr-line-segs 8))))
            (if (and (< 0 (window-workspace-num curr-window)) ; Sticky window
                     (or (null workspace)
                         (= workspace (window-workspace-num curr-window)))
                     (not (string= "Desktop" (window-name curr-window)))
                     (and (plusp (window-width curr-window))
                          (plusp (window-height curr-window))))
              (push curr-window windows))))))
    (nreverse windows)))

(defun get-focused-window-id ()
  "Get the id of the currently focused window in hexadecimal.
   The final format of the number is intended to be compatible with wmctrl.
   E.g.: 0x12345678."
  (let* ((cmd-r (run-cmd "xdotool getwindowfocus")))
    (if (failed? cmd-r)
      cmd-r
      (new-r :success "" (format nil "0x~8,'0X"
                                 (loose-parse-int (r-data cmd-r)))))))

;;; Helper Functions ===========================================================

;;; Public Functions -----------------------------------------------------------

(defun go-to-workspace (num)
  "Go to workspace number `num` (1-based index)."
  (if (non-positive? num)
    (return-from go-to-workspace (new-r :error
                                        "Workspace must be a postive integer")))
  (save-active-workspace-num (get-active-workspace-num))
  (run-cmd (sf "wmctrl -s ~A" (1- (as-safe-wmctrl-workspace-num num)))))

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

(defun focus-window (pos)
  "Focus the window specified by the relative position, `pos` (i.e. up, down,
   left, right)."
  (let* ((get-focused-window-id-r (get-focused-window-id))
         (focused-window-id "")
         (focused-window nil)
         (focused-window-position -1)
         (windows '())
         (position-of-window-to-focus -1))

    (if (failed? get-focused-window-id-r)
      (return-from focus-window get-focused-window-id-r))

    (setf focused-window-id (r-data get-focused-window-id-r))

    (cond (;; Focus UP window 
           (string-equal "up" pos)
           (setf windows (sort (list-windows
                                 :workspace (get-active-workspace-num))
                               #'<
                               :key #'window-y-offset))
           (setf focused-window
                 (find focused-window-id
                       windows
                       :key #'window-id
                       :test #'string-equal))
           (setf focused-window-position (position focused-window windows))
           (setf position-of-window-to-focus
                 (if (zerop focused-window-position)
                   (1- (length windows))
                   (1- focused-window-position))))

          ;; Focus DOWN window
          ((string-equal "down" pos)
           (setf windows (sort (list-windows
                                 :workspace (get-active-workspace-num))
                               #'<
                               :key #'window-y-offset))
           (setf focused-window
                 (find focused-window-id
                       windows
                       :key #'window-id
                       :test #'string-equal))
           (setf focused-window-position (position focused-window windows))
           (setf position-of-window-to-focus
                 (if (>= focused-window-position (1- (length windows)))
                   0
                   (1+ focused-window-position))))

          ;; Focus LEFT window 
          ((string-equal "left" pos)
           (setf windows (sort (list-windows
                                 :workspace (get-active-workspace-num))
                               #'<
                               :key #'window-x-offset))
           (setf focused-window
                 (find focused-window-id
                       windows
                       :key #'window-id
                       :test #'string-equal))
           (setf focused-window-position (position focused-window windows))
           (setf position-of-window-to-focus
                 (if (zerop focused-window-position)
                   (1- (length windows))
                   (1- focused-window-position))))

          ;; Focus RIGHT window
          ((string-equal "right" pos)
           (setf windows (sort (list-windows
                                 :workspace (get-active-workspace-num))
                               #'<
                               :key #'window-x-offset))
           (setf focused-window
                 (find focused-window-id
                       windows
                       :key #'window-id
                       :test #'string-equal))
           (setf focused-window-position (position focused-window windows))
           (setf position-of-window-to-focus
                 (if (>= focused-window-position (1- (length windows)))
                   0
                   (1+ focused-window-position))))
          ;; Illegal argument
          (t (return-from
               focus-window
               (new-r :error
                      (sf "Unrecognised relative window position '~A'" pos)))))

    (if (negative? position-of-window-to-focus)
      (return-from
        focus-window
        (new-r :error "Failed to determine position of window to focus.")))

    (let* ((window-to-focus (nth position-of-window-to-focus windows))
           (cmd (sf "wmctrl -i -a ~A" (window-id  window-to-focus))))
      (run-cmd cmd))))

;;; Public Functions ===========================================================
