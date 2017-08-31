(in-package :glu)

(annot:enable-annot-syntax)

@export
(defvar *SIGINT* 2)

@export
(defmacro handle-signal (signo &body body)
  "Handle Unix signal.
   Note that `signo` must be a simple integer. I.e. it shouldn't even be a
   function that evaluates to an integer as this macro doesn't support this.
   Taken from https://stackoverflow.com/a/10442062/24318."
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
                         (declare (ignore signo))
                         ,@body)
       (cffi:foreign-funcall "signal"
                             :int ,signo
                             :pointer (cffi:callback ,handler)))))

@export
(defun run-cmd (cmd)
  "Run command specified by `CMD'.
   A result object is returned."
  (multiple-value-bind (std-out std-err ret-val)
      (uiop:run-program cmd
                        :ignore-error-status t
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t))
    (if (zerop ret-val)
        (r=> :success "" std-out)
        (r=> :error
             (sf "ERROR ~A: ~A"
                 ret-val
                 (if (and (empty? std-out) (empty? std-err))
                     "unknown (cmd reported no info)"
                     (or std-err std-out)))))))
