(in-package :glu)

(annot:enable-annot-syntax)

@export
(defvar empty/*objects*
  (let ((hash (make-hash-table)))
    (setf (gethash 'pathname hash) #P"")
    hash)
  "A hash table of 'empty' objects, keyed by their respective type.")

@export
(defun empty (type &key unless)
  "Get the registered 'empty' object (if any) unless `UNLESS` is non-nil.
   This is an idiomatic way of ensuring the given `UNLESS` object is always
   non-nil."
  (if (not (null unless))
    (return-from empty unless))

  (if (stringp unless)
    (return-from empty unless))

  (if (eq 'string type)
    (return-from empty ""))

  (gethash type empty/*objects*))

@export
(defun empty=> (obj)
  "Register `OBJ` as the canonical 'empty' instance of it's respective type."
  (setf (gethash (type-of obj) empty/*objects*) obj))

@export
(defgeneric empty? (obj)
  (:documentation
    "Determine whether `OBJ` is considered an 'empty' instance of it's
     respective data-type. E.g. a null object, an empty string, an empty list,
     etc."))

@export
(defmethod empty? ((obj pathname))
  (zerop (length (princ-to-string obj))))

@export
(defmethod empty? ((obj string))
  (zerop (length obj)))

@export
(defmethod empty? ((obj list))
  (zerop (length obj)))

@export
(defmethod empty? ((obj t))
  (or (null obj)
      (eq obj (empty (type-of obj)))))
