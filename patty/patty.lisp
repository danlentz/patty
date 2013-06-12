
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:patty)

(defmacro make (class &rest initargs)
  `(make-instance ',class ,@initargs))

(defmacro defmethods (function-name &body clauses)
  (let ((where-funs nil)
	(macros nil)
	(symbol-macros nil)
	(methods nil))
    (loop for clause in clauses
	  for clause-name = (symbol-name (car clause))
	  do (cond
	       ((string= "=" clause-name)
		(push `(defmethod ,function-name ,@(cdr clause)) methods))
	       ((string-equal "where" clause-name)
		(push (cdr clause) where-funs))
	       ((string-equal "syntax" clause-name)
		(push (cdr clause) macros))
	       ((string-equal "expr" clause-name)
		(push (cdr clause) symbol-macros))
	       (t (error "Unknown clause name ~S." clause-name))))
    `(symbol-macrolet ,(nreverse symbol-macros)
      (macrolet ,(nreverse macros)
	(labels ,(nreverse where-funs)
	  ,@(nreverse methods))))))

(defun make-defclass-slot-list (slot-expr)
  (destructuring-bind (reader &key type initarg equality) slot-expr
    (let ((slot-name (intern (concatenate 'string
					  "<" (symbol-name reader) ">")
			     (symbol-package reader)))
	  (res nil)
	  (initarg (or initarg (intern (symbol-name reader) :keyword))))
      (when type
	(push type res)
	(push :type res))
      (when equality
	(push equality res)
	(push :equality res))
      (push initarg res)
      (push :initarg res)
      (push `(error "~S required" ,initarg) res)
      (push :initform res)
      (push reader res)
      (push :reader res)
      (push slot-name res)
      res)))

(defmacro defdata-type (name direct-superclasses &rest clauses)
  (let ((slots nil)
	(slot-infos nil)
	(options (list '(:metaclass data-type-class))))
    (when (stringp (car clauses))
      (push `(:documentation ,(pop clauses)) options))
    (loop for clause in clauses
	  for clause-name = (car clause)
	  do (ecase clause-name
	       (:slots
		(dolist (slot-expr (cdr clause))
		  (multiple-value-bind (slot-def slot-info)
		      (make-defclass-slot-list slot-expr)
		    (push slot-def slots)
		    (push slot-info slot-infos))))))
    `(defclass ,name ,direct-superclasses
      ,slots
      ,@options)))

(defmacro defdata-unique (name direct-superclasses &rest clauses)
  (let ((options (list '(:metaclass data-unique-class)))
	(equal-fun nil))
    (when (stringp (car clauses))
      (push `(:documentation ,(pop clauses)) options))
    (loop for clause in clauses
	  for clause-name = (car clause)
	  do (ecase clause-name
	       (:derive-equal
		(when equal-fun (error "Only one :derive-equal clause allowed."))
		(setf equal-fun (second clause)))))
    `(progn
      (defclass ,name ,direct-superclasses
	()
	,@options)
      ,(when equal-fun
         `(defmethod ,equal-fun ((a ,name) (b ,name)) t)))))

(defmacro defdata-object (name direct-superclasses &rest clauses)
  (let ((slots nil)
	(slot-infos nil)
	(equal-fun nil)
	(options (list '(:metaclass data-object-class))))
    (when (stringp (car clauses))
      (push `(:documentation ,(pop clauses)) options))
    (loop for clause in clauses
	  for clause-name = (car clause)
	  do (ecase clause-name
	       (:slots
		(dolist (slot-expr (cdr clause))
		  (multiple-value-bind (slot-def slot-info)
		      (make-defclass-slot-list slot-expr)
		    (push slot-def slots)
		    (push slot-info slot-infos))))
	       (:derive-equal
		(when equal-fun (error "Only one :derive-equal clause allowed."))
		(setf equal-fun (second clause)))))
    (when equal-fun
      (push `(:equal-functions ,equal-fun) options))
    `(progn
      (defclass ,name ,direct-superclasses
	,slots
	,@options))))
