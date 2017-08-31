(in-package :cl-user)

;;; Reader Macros --------------------------------------------------------------

;; Allow the lambda character 'λ' to be used in place of the word lambda, for
;; brevity's sake.
;; Note that we intentionally define this in the :CL-USER package
(set-macro-character #\λ
                     (lambda (stream char)
                       (declare (ignore char stream))
                       (quote lambda)))

;; Create a list with braces. I specifically wanted this to more easily create
;; and identify plists.
(set-macro-character #\{ (lambda (stream char)
                           (declare (ignore char))
                           (loop :for (k v)
                                 :on (read-delimited-list #\} stream t)
                                 :with plist = (list 'list)
                                 :do (push k plist)
                                 :finally (return (nreverse plist)))))
(set-macro-character #\} (get-macro-character #\) nil))

;;; Reader Macros ==============================================================

;;; Package Definition ---------------------------------------------------------

(defpackage :glu
  (:documentation "Global Lisp Utilities") 
  (:use :cl :local-time))

(in-package :glu)

(annot:enable-annot-syntax)

;;; Package Definition =========================================================

(defvar *start-time* (get-internal-real-time))

@export
(defvar *english-list*
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~}"
  "A control string to format a list of items in a friendly manner.
   E.g. '1 and 2', or '1, 2 and 3'.
   This was taken from the book Practical Common Lisp.")

@export
(defmacro export-struct (name &rest fields)
  "Export the struct with the given name, along with the default names of the
   constuctor, copier and type predicate check. E.g. if `name` was `'circle`,
   the following symbols are exported:
   * `CIRCLE`
   * `MAKE-CIRCLE`
   * `COPY-CIRCLE`
   * `CIRCLE-P`
   If `fields` is given it should be a list of the struct's fields.
   E.g. call `(export-struct 'circle 'radius 'circumference)`."
  `(progn
     (export (intern (princ-to-string ,name)))
     (export (intern (format nil "MAKE-~A" ,name)))
     (export (intern (format nil "COPY-~A" ,name)))
     (export (intern (format nil "~A-P" ,name)))
     ,@(if (plusp (length fields))
           (loop :for f :in fields
                 :collect
                 `(export (intern (format nil "~A-~A" ,name ,f)))))))

@export
(defmacro => (obj val prop)
  "Sets the value of an object's slot/property."
  `(progn
     (setf (getf ,obj ,prop) ,val)
     (values ,obj ,val)))

@export
(defmacro 1st (obj)
  "Gets the first item in OBJ if it's a list, otherwise OBJ is simply returned."
  `(if (listp ,obj) (first ,obj) ,obj))

@export
(defmacro 2nd (obj &optional fallback)
  "Gets the second item in OBJ if it's a list of at least two items, otherwise
   FALLBACK."
  `(cond ((atom ,obj)
          ,fallback)
         ((and (listp ,obj) (> (length ,obj) 1))
          (second ,obj))
         (t ,fallback)))

@export
(defmacro 3rd (obj &optional fallback)
  "Gets the third item in OBJ if it's a list of at least three items, otherwise
   FALLBACK."
  `(cond ((atom ,obj)
          ,fallback)
         ((and (listp ,obj) (> (length ,obj) 2))
          (third ,obj))
         (t ,fallback)))

@export
(defmacro last1 (lst)
  "Get the last item in lst. If lst is not a list it simply returns it."
  `(if (listp ,lst)
     (car (last ,lst))
     ,lst))

@export
(defun blank? (str)
  "Determine whether `STR` contains only whitespace characters."
  (or (empty? str)
      (cl-ppcre:scan "^\\s+$" str)))

@export
(defmacro join (arg1 arg2 &rest args)
  "Concatenate the given lists, strings or objects. The branching is based on
   the type of the first argument. If the first argument is a string the result
   is a single string, rather than a list of strings."
  `(cond ((stringp ,arg1)
          (format nil "~{~A~}" (list ,arg1 ,arg2 ,@args)))
         ((listp ,arg1)
          (concatenate 'list ,arg1 ,arg2 ,@args))
         (t
          (concatenate 'list (list ,arg1 ,arg2 ,@args)))))

@export
(defmacro trim (str &key (char #\space) left-only right-only)
  "Trim `STR` of `CHAR`."
  `(cond (,left-only
           (string-left-trim (list ,char) ,str))
         (,right-only
           (string-right-trim (list ,char) ,str))
         (t
          (string-trim (list ,char) ,str))))

@export
(defmacro starts-with (test input)
  "Test whether `input` starts with `test`."
  `(string= ,test
            (subseq ,input 0 (min (length ,input) (length ,test)))))

@export
(defmacro loose-parse-int (str &key (fallback 0))
  "Very lenient parsing of STR to an integer."
  `(cond ((typep ,str 'integer)
          ,str)
         ((empty? ,str)
          ,fallback)
         (t
          (or (parse-integer (if (typep ,str 'string)
                                 ,str
                                 (to-string ,str))
                             :junk-allowed t)
              ,fallback))))

@export
(defmacro sf (control-string &rest args)
  "Convenience macro to format a string. `sf` stands for 'string format'."
  (if (listp control-string)
      `(format nil (format nil "~{~A~}" ,control-string) ,@args)
      `(format nil ,control-string ,@args)))

@export
(defmacro to-string (obj)
  "Shortcut of `PRINC-TO-STRING'"
  `(princ-to-string ,obj))

@export
(defmacro labeled-time (form)
  "Shows timing info via (time), prefixed with the form it's called for.
   This was taken from the book Practical Common Lisp."
  `(progn
    (format *trace-output* "~2&~a" ',form)
    (time ,form)))

@export
(defun get-run-time ()
  "Get the amount of time elapsed since the program started in seconds."
  (/ (- (get-internal-real-time) *start-time*) internal-time-units-per-second))

@export
(defun display-run-time (&optional app-name
                                   (total-seconds 0 total-seconds-given?))
  "Display the amount of time this app has run for."
  (let* ((total-seconds (if total-seconds-given? total-seconds (get-run-time)))
         (days (floor (/ total-seconds 60 60 24)))
         (hours (- (floor (/ total-seconds 60 60))
                   (* days 24)))
         (minutes (- (floor (/ total-seconds 60))
                     (* days 24 60)
                     (* hours 60)))
         (seconds (- (floor total-seconds)
                     (* days 24 60 60)
                     (* hours 60 60)
                     (* minutes 60)))
         (millis (floor (* 1000 (nth-value 1 (floor total-seconds))))))
    (format t "~%~A (clock) runtime: " (if (empty? app-name) "App" app-name))
    (if (>= days 1)
      (format t "~:D day~:P " days))
    (format t "~2,'0D:~2,'0D:~2,'0D.~2,'0D.~%" hours minutes seconds millis)))

@export
(defun pretty-time (time)
  "Formats a date/time to a user-friendly form. TIME is expected to either be a
   timestamp readable by LOCAL-TIME, or a LOCAL-TIME:TIMESTAMP object."
  (if (empty? time)
      ""
      (let* ((format-desc '())
             (timestamp (if (stringp time)
                            (parse-timestring time)
                            time)))

        (setf format-desc '(:short-weekday " " :short-month " " :day " "
                            :year ", " :hour12 ":" (:min 2) " " :ampm))

        (format-timestring nil timestamp :format format-desc))))

@export
(defmacro positive? (num)
  "Determine whether `num` is a positive number."
  `(<= 1 ,num))

@export
(defmacro non-positive? (num)
  "Determine whether `num` is a non-positive number."
  `(>= 0 ,num))

@export
(defmacro non-negative? (num)
  "Determine whether `num` is a non-negative number."
  `(<= 0 ,num))

@export
(defmacro negative? (num)
  "Determine whether `num` is a negative number."
  `(>= -1 ,num))

@export
(defun next-item-idx (curr-item-idx all-items &key (direction 'right))
  "Get the index of the next item.
   `curr-item-idx` specifies the 0-based index of the current item.
   `all-items` is either a list or the length of a list.
   `direction` can be left/up or right/down."
  (let* ((list-len (if (listp all-items)
                       (length all-items)
                       all-items)))
    (if (or (string-equal 'right direction)
            (string-equal 'down direction))
        (if (>= curr-item-idx (1- list-len))
            0
            (1+ curr-item-idx))
        (if (zerop curr-item-idx)
            (1- list-len)
            (1- curr-item-idx)))))
