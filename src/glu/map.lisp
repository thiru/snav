#||
* Contains utilties for working with maps (plists).
* Wherever map is used it is implied that the underlying implementation is a plist.
||#

(in-package :glu)

(annot:enable-annot-syntax)

@export
(defun -> (obj &rest props)
  "Gets the value of a key in a map, with potentially nested maps."
  (loop :for curr-prop :in props
        :with curr-obj = obj
        :do (setf curr-obj (getf curr-obj curr-prop))
        :finally (return curr-obj)))

(defun m/build-inner-setf (var-name props lst)
  "Builds the inner sexp used by `==>`."
  (if (null lst)
      (setf lst var-name))
  (if (zerop (length props))
      lst
      (m/build-inner-setf var-name (cdr props) (list 'getf lst (first props)))))

@export
(defmacro ==> (map val &rest more-props)
  "Sets the value of a nested map value."
  `(progn
     (cond ((zerop (length ',more-props))
            (setf ,map ,val))
           (t
            (setf ,(m/build-inner-setf map more-props nil) ,val)))))

@export
(defun m? (obj)
  "Does `obj` appear to be a map?"
  (and (listp obj)
       (not (empty? obj))
       (keywordp (1st obj))
       (evenp (length obj))))

@export
(defvar m/*specs* (list)
  "Contains a map of map specifications.")

@export
(defmacro m/names (map)
  "Get the names of all keys in the given map."
  `(loop :for (k v) :on ,map :by #'cddr
         :collect k))

@export
(defun m/print (map &key (depth 0) (stream t) max-depth)
  "Print map in a more human readable form, but still support being read by the
   lisp reader."
  (if (null map)
      (return-from m/print))
  (let* ((outer-indent (make-string (* 2 depth) :initial-element #\space))
         (inner-indent (make-string (* 2 (1+ depth)) :initial-element #\space)))
    (format stream "~A(~%" outer-indent)
    (loop :for (k v) :on map :by #'cddr
          :for i :from 0
          :do
          (princ inner-indent stream)
          (write k :stream stream :readably t)
          (if (and (m? v) (or (null max-depth) (< depth max-depth)))
              (progn
                (princ #\linefeed stream)
                (m/print v :depth (1+ depth) :stream stream))
              (progn
                (princ " " stream)
                (write v :stream stream :readably t)
                (princ #\linefeed stream))))
    (format stream "~A)~%" outer-indent)))

@export
(defun m/def (name doc map)
  "Define a well-formed map. If the given map is well formed it is also stored
   in `m/*specs*`. Otherwise an `r` is returned."
  (if (not (keywordp name))
      (return-from
        m/def
        (r=> :error "Name must be a keyword symbol.")))
  (if (empty? map)
      (return-from
        m/def
        (r=> :error "A well-formed map can't be empty")))
  (if (not (evenp (length map)))
      (return-from
        m/def
        (r=> :error "A well-formed map must have an even number of items.")))
  (loop :for (k v) :on map :by #'cddr
        :for i :from 0
        :do
        (if (not (keywordp k))
            (return-from
              m/def
              (r=> :error
                   (sf '("A well-formed map must have a keyword symbol at "
                         "every even position. The item at index ~A (~A) "
                         "violates this rule.")
                       i k)))))
  (=> m/*specs* { :doc doc :spec map } name))

@export
(defun m/getspec (name)
  "Get the map specification with the given name. The documentation string is
   returned as a second value."
  (let* ((spec-with-doc (-> m/*specs* name)))
    (values (-> spec-with-doc :spec) (-> spec-with-doc :doc))))

@export
(defun m/new (name)
  "Create a new map based on the spec with the given name.
   This assumes the spec has been previously registed with `m/def`."
  (let* ((map-spec (m/getspec name)))
    (loop :for (k v) :on map-spec :by #'cddr
          :with new-map
          :do
          (if (listp v)
              (=> new-map (-> v :val) k)
              (=> new-map v k))
          :finally (return new-map))))
