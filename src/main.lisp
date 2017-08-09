(in-package :snav)

;;; Structs --------------------------------------------------------------------

(defstruct app-info
  (name "snav")
  (debug-mode? nil)
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

(defstruct monitor
  (id "" :type string)
  (width 0 :type integer)
  (height 0 :type integer)
  (x-offset 0 :type integer)
  (y-offset 0 :type integer))

;;; Structs ====================================================================

;;; Init -----------------------------------------------------------------------

(defun create-app-info ()
  "Initialise the app."
  (let* ((version-file-path (asdf:system-relative-pathname :snav "version")))
    (make-app-info :debug-mode? nil
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

(defun window-maximised? (id)
  "Determine whether the window with the specified id is maximized
   (horizontally and vertically)."
  (if (empty? id)
      (return-from
        window-maximised?
        (new-r :error "No window id specified.")))

  (let* ((cmd-r (run-cmd (sf "xwininfo -wm -id ~A" id)))
         (cmd-output-lines '())
         (maximised-horz? nil)
         (maximised-vert? nil))

    (if (failed? cmd-r)
        (return-from
          window-maximised?
          cmd-r))

    (setf cmd-output-lines (split-sequence #\linefeed (r-data cmd-r)))

    (loop :for line in cmd-output-lines
          :for i :from 0
          :when (starts-with "Maximized " (trim #\space line))
          :do
          (if (string-equal "Maximized Horz" (trim #\space line))
              (setf maximised-horz? t))
          (if (string-equal "Maximized Vert" (trim #\space line))
              (setf maximised-vert? t)))
    
    (if (and maximised-horz? maximised-vert?)
        (new-r :success "Yes" t)
        (new-r :success "No" nil))))

(defun window-in-monitor? (window monitor)
  "Determine whether `window` is in `monitor`. This is defined as the top-left
   corner of the window residing within the monitor."
  (and (>= (window-x-offset window)
           (monitor-x-offset monitor))
       (<= (window-x-offset window)
           (+ (monitor-x-offset monitor)
              (monitor-width monitor)))
       (>= (window-y-offset window)
           (monitor-y-offset monitor))
       (<= (window-y-offset window)
           (+ (monitor-y-offset monitor)
              (monitor-height monitor)))))

(defun correct-window-x-offset-snap (x-offset monitor)
  "Account for minor discrepancies in x offset when moving windows between
   monitors. We state that if the window is within 3 pixels of the edge of the
   monitor, than we snap it to that edge."
  (if (>= 3 (abs (- x-offset (monitor-x-offset monitor))))
      (monitor-x-offset monitor)
      x-offset))

(defun correct-window-y-offset-snap (y-offset monitor)
  "Account for minor discrepancies in y offset when moving windows between
   monitors. We state that if the window is within 3 pixels of the edge of the
   monitor, than we snap it to that edge."
  (if (>= 3 (abs (- y-offset (monitor-y-offset monitor))))
      (monitor-y-offset monitor)
      y-offset))

(defun move-mouse (x y &key window)
  "Move the mouse to the specified coorindates.
   If `window` is specified the coordinates are relative to the window, with
   0,0 being the top-left corner of the window. Otherwise the coordinates are
   relative to the entire screen (which may encompass all monitors)."
  (if (empty? window)
      (run-cmd (sf "xdotool mousemove ~A ~A" x y))
      (run-cmd (sf "xdotool mousemove --window ~A ~A ~A"
                   (window-id window) x y))))

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
(defun list-workspaces ()
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

(defun list-monitors ()
  "Get a list of monitors from `xrandr --current` command. The `--current`
   option gets the current screen configuration without polling for hardware
   changes."
  (let* ((list-monitors-cmd-r (run-cmd "xrandr --current"))
         (cmd-output-lines '())
         (monitors '()))
    (if (succeeded? list-monitors-cmd-r)
        (progn
          (setf cmd-output-lines
                (split-sequence #\linefeed (r-data list-monitors-cmd-r)))
          (dolist (line cmd-output-lines)
            (multiple-value-bind
              (whole-match sub-matches)
              (cl-ppcre:scan-to-strings
                "^(\\S+) connected (\\d+)x(\\d+)\\+(\\d+)\\+(\\d+)"
                line)
              (declare (ignore whole-match))
              (when (= 5 (length sub-matches))
                (push (make-monitor :id (aref sub-matches 0)
                                    :width
                                    (loose-parse-int (aref sub-matches 1))
                                    :height
                                    (loose-parse-int (aref sub-matches 2))
                                    :x-offset
                                    (loose-parse-int (aref sub-matches 3))
                                    :y-offset
                                    (loose-parse-int (aref sub-matches 4)))
                      monitors))))))
    (nreverse monitors)))

;;; Helper Functions ===========================================================

;;; Public Functions -----------------------------------------------------------

(defun show-windows ()
  "Show windows across all workspaces."
  (let* ((windows '())
         (focused-win-cmd-r (get-focused-window-id)))
    (dolist (window (list-windows))
      (push (format nil
                    "~A ~A ~A"
                    (window-workspace-num window)
                    (if (and (succeeded? focused-win-cmd-r)
                             (string-equal (r-data focused-win-cmd-r)
                                           (window-id window)))
                        "*"
                        "-")
                    (window-name window))
            windows))
    (new-r :success "Successfully listed windows" (nreverse windows))))

(defun focus-window (direction)
  "Focus the window specified by the direction (i.e. up, down, left, right)."
  (let* ((get-focused-window-id-r (get-focused-window-id))
         (focused-window-id "")
         (focused-window nil)
         (focused-window-position -1)
         (windows '())
         (position-of-window-to-focus -1)
         (curr-workspace -1))

    (if (failed? get-focused-window-id-r)
      (return-from focus-window get-focused-window-id-r))

    (setf focused-window-id (r-data get-focused-window-id-r))
    (setf curr-workspace (get-active-workspace-num))

    (cond (;; Focus UP window 
           (string-equal "up" direction)
           (setf windows
                 (stable-sort (list-windows :workspace curr-workspace)
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
          ((string-equal "down" direction)
           (setf windows
                 (stable-sort (list-windows :workspace curr-workspace)
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
          ((string-equal "left" direction)
           (setf windows
                 (stable-sort (list-windows :workspace curr-workspace)
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
          ((string-equal "right" direction)
           (setf windows
                 (stable-sort (list-windows :workspace curr-workspace)
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
                      (sf "Unrecognised window direction '~A'" direction)))))

    (if (negative? position-of-window-to-focus)
      (return-from
        focus-window
        (new-r :error "Failed to determine position of window to focus.")))

    (let* ((window-to-focus (nth position-of-window-to-focus windows))
           (cmd (sf "wmctrl -i -a ~A" (window-id  window-to-focus))))
      (run-cmd cmd))))

(defun move-window (direction)
  "Move the window to the monitor in the specified direction. The mouse is also
   moved to top-left corner of the focused window."
  (let* ((monitors (list-monitors))
         (get-focused-window-id-r nil)
         (focused-window-id "")
         (focused-window nil)
         (curr-mon nil)
         (next-mon nil)
         (new-x-pos -1)
         (new-y-pos -1)
         (maximised-check-r nil))

    (if (empty? monitors)
        (return-from
          move-window
          (new-r :success "Only one monitor so not moving window")))

    (setf get-focused-window-id-r (get-focused-window-id))

    (if (failed? get-focused-window-id-r)
      (return-from move-window get-focused-window-id-r))

    (setf focused-window-id (r-data get-focused-window-id-r))

    (setf focused-window
          (find focused-window-id
                (list-windows)
                :key #'window-id
                :test #'string-equal))

    (if (empty? focused-window)
        (return-from
          move-window
          (new-r :error
                 (sf "Failed to determine focused window (id ~A)"
                     focused-window-id))))

    (cond (;; Move window UP/DOWN
           (or (string-equal 'up direction)
               (string-equal 'down direction))
           (setf monitors (stable-sort monitors #'< :key #'monitor-y-offset))
           (loop :for monitor in monitors
                 :for i :from 0
                 :when (window-in-monitor? focused-window monitor)
                 :do
                 (setf curr-mon monitor)
                 (setf next-mon (nth (next-item-idx i
                                                    monitors
                                                    :direction direction)
                                     monitors))
                 (if (not (= (monitor-y-offset curr-mon)
                             (monitor-y-offset next-mon)))
                     (setf new-y-pos
                           (correct-window-y-offset-snap
                             (+ (monitor-y-offset next-mon)
                                (- (window-y-offset focused-window)
                                   (monitor-y-offset curr-mon)))
                             next-mon)))))

          ;; Move window LEFT/RIGHT
          ((or (string-equal 'left direction)
               (string-equal 'right direction))
           (setf monitors (stable-sort monitors #'< :key #'monitor-x-offset))
           (loop :for monitor in monitors
                 :for i :from 0
                 :when (window-in-monitor? focused-window monitor)
                 :do
                 (setf curr-mon monitor)
                 (setf next-mon (nth (next-item-idx i
                                                    monitors
                                                    :direction direction)
                                     monitors))
                 (if (not (= (monitor-x-offset curr-mon)
                             (monitor-x-offset next-mon)))
                     (setf new-x-pos (correct-window-x-offset-snap
                                       (+ (monitor-x-offset next-mon)
                                          (- (window-x-offset focused-window)
                                             (monitor-x-offset curr-mon)))
                                       next-mon)))))

          ;; Illegal argument
          (t (return-from
               move-window
               (new-r :error
                      (sf "Unrecognised window direction '~A'" direction)))))

    ;; Do we have valid x, y coordinates?
    (if (or (non-negative? new-x-pos)
            (non-negative? new-y-pos))
        (progn
          (setf maximised-check-r (window-maximised? focused-window-id))
          (run-cmd (sf "wmctrl -r :ACTIVE: -e 0,~A,~A,-1,-1" new-x-pos new-y-pos))
          (move-mouse 0 0 :window focused-window)
          (if (failed? maximised-check-r)
              (return-from move-window maximised-check-r)
              (if (r-data maximised-check-r)
                  (run-cmd (join "wmctrl -r :ACTIVE: "
                                 "-b add,maximized_vert,maximized_horz")))))
        (new-r :success
               (sf "No valid monitor in direction '~A' to move to."
                   direction)))))

(defun show-workspaces ()
  "Show workspaces."
  (let* ((workspaces '()))
    (dolist (workspace (list-workspaces))
      (push (format nil
                    "~A ~A ~A"
                    (workspace-num workspace)
                    (if (workspace-active? workspace) "*" "-")
                    (workspace-name workspace))
            workspaces))
    (new-r :success "Successfully listed workspaces" (nreverse workspaces))))

(defun go-to-workspace (num)
  "Go to workspace number `num` (1-based index)."
  (if (non-positive? num)
    (return-from go-to-workspace (new-r :error
                                        "Workspace must be a postive integer")))
  ;; Don't save last active workspace it is the same as the target, `num`
  (let* ((active-workspace (get-active-workspace-num)))
    (if (not (= num active-workspace))
      (save-active-workspace-num active-workspace)))
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

(defun show-monitors ()
  "Show connected monitors."
  (let* ((monitors '()))
    (dolist (monitor (list-monitors))
      (push (format nil
                    "~A ~Ax~A +~A,~A"
                    (monitor-id monitor)
                    (monitor-width monitor)
                    (monitor-height monitor)
                    (monitor-x-offset monitor)
                    (monitor-y-offset monitor))
            monitors))
    (new-r :success "Successfully listed monitors" (nreverse monitors))))

;;; Public Functions ===========================================================
