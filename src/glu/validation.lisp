(in-package :glu)

(annot:enable-annot-syntax)

@export
(defparameter
  levels
  '(:success 2 :info 1 :debug 0 :warning -1 :error -2 :fatal -3)
  "General levels that can be used for logging, error reporting, etc.")

(m/def
  :r
  "Encapsulates a 'result' indicating the success/failure state of a function
   or operation. An optional data object specifies the 'natural' return type of
   the function."
  {
    :level :info
    :message ""
    :data nil
  })

@export
(defun r=> (&optional level msg data)
  "Creates a new 'result' object."
  (list :level (or level :info) :message msg :data data))

@export
(defun succeeded? (obj &optional (min-level :debug))
  "Determine whether `obj` represents a 'successful' object."
  (if (m? obj)
      (<= (or (-> levels min-level) 0) (or (-> levels (-> obj :level)) -3))
      obj))

@export
(defun failed? (obj &optional (min-level :debug))
  "Determine whether `obj` represents a 'failed' object."
  (not (succeeded? obj min-level)))
