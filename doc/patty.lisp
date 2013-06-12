(asdf:oos 'asdf:load-op "patty")
(use-package :patty)

(defdata-type path ())

(defdata-type pathroot (path)
  "A pathroot can name a host, or a drive, or the root directory of a
  unix filesystem, or the current directory, etc.")

(defun list-of-strings-p (list)
  (or (null list)
      (and (stringp (car list)) (list-of-strings-p (cdr list)))))

(deftype pathlist () '(and list (satisfies list-of-strings-p)))

(defgeneric pathroot (path)
  (:documentation "Get the root of a path. The result is of type pathroot.")
  (:method ((path pathroot))
    path))

(defgeneric pathlist (path)
  (:documentation "Get the directory/filename list, of type pathlist.")
  (:method ((_ pathroot))
    nil))

(defgeneric absolutep (path)
  (:documentation "Is this path absolute?"))

(defgeneric join-paths (a b)
  (:documentation "Get a path that refers to the file/directory b under the directory a")
  (:method ((a path) (b path))
    (error "Can't join pathes ~S and ~S." a b)))

(defgeneric path= (a b)
  (:documentation "Are the paths a and b equal?")
  (:method ((a path) (b path))
    nil))

(defgeneric native-path (path)
  (:documentation "Convert path into a string in the platforms native filename syntax."))

(defdata-type unix-path (path))
(defdata-type relative-path (unix-path))
(defdata-type absolute-path (unix-path))

(defdata-unique root-dir (pathroot absolute-path)
  (:derive-equal path=))

(defdata-unique current-dir (pathroot relative-path)
  (:derive-equal path=))

(deftype not-empty-pathlist () '(and cons pathlist))

(defdata-type regular-path (unix-path)
  (:slots (pathlist :type not-empty-pathlist
                    :equality equal)))

(defdata-object regular-absolute-path (regular-path absolute-path)
  (:derive-equal path=))

(defdata-object regular-relative-path (regular-path relative-path)
  (:derive-equal path=))

(defmethods pathroot
  (= ((_ relative-path))
     (make current-dir))
  (= ((_ absolute-path))
     (make root-dir)))

(defmethods absolutep
  (= ((_ relative-path))
     nil)
  (= ((_ absolute-path))
     t))

(defmethods join-paths
  (= ((a absolute-path) (b regular-relative-path))
     (mk regular-absolute-path))
  (= ((a relative-path) (b regular-relative-path))
     (mk regular-relative-path))
  (syntax mk (regclass)
          `(make ,regclass :pathlist (append (pathlist a) (pathlist b)))))

(defmethods native-path
  (= ((_ current-dir))
     ".")
  (= ((path unix-path))
     (with-output-to-string (res)
       (when (absolutep path) (write-char #\/ res))
       (labels ((out (components)
                  (write-string (car components) res)
                  (when (cdr components)
                    (write-char #\/ res)
                    (out (cdr components)))))
         (when (pathlist path) (out (pathlist path)))))))

(defmethod print-object ((path path) stream)
  (write `(path ,(native-path path)) :stream stream))

(defmethods foo
  (= ((a string) (b string))
     (wr "strings" a b len))
  (= ((a list) (b list))
     (wr "lists" a b len))
  (where wr (type a b combined-length)
         (format t "~A ~S ~S, combined length: ~A~%" type a b combined-length))
  (expr len (+ (length a) (length b))))

