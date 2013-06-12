
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:patty.mop)

(defclass patty-class (finalize-class-mixin)
  ((equal-functions
    :initarg :equal-functions
    :initform nil
    :reader class-equal-functions)))

(defmethod validate-superclass ((class patty-class)
				(superclass standard-class))
  (finalize-inheritance superclass)
  (or (typep superclass 'patty-class)
      (not (class-has-slot-writer-p superclass))))

(defclass patty-slot-definition (standard-slot-definition)
  ((equality
    :initarg :equality
    :initform 'eql
    :reader slot-equality)))

(defmethod slot-equality ((slot standard-slot-definition))
  ;; This will change soon
  'eql)

(defmethod initialize-instance :after ((slot patty-slot-definition)
				       &key (equality nil ep) &allow-other-keys)
  (when ep
    (setf (slot-value slot 'equality) equality)))

(defclass patty-direct-slot-definition (patty-slot-definition
					standard-direct-slot-definition)
  ())

(defclass patty-effective-slot-definition (patty-slot-definition
					   standard-effective-slot-definition)
  ())

(defmethod compute-effective-slot-definition :around ((class patty-class)
						      slot-name direct-slots)
  (declare (ignore slot-name))
  ;; the first slot-definition in direct-slots is the most specific
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'equality) (slot-equality (first direct-slots)))
    slot))

(defmethod direct-slot-definition-class ((class patty-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'patty-direct-slot-definition))

(defmethod effective-slot-definition-class ((class patty-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'patty-effective-slot-definition))

(defclass data-type-class (patty-class abstract-class)
  ())

(defclass data-unique-class (patty-class singleton-class)
  ())

(defmethod validate-superclass ((class data-unique-class)
				(superclass standard-class))
  (and (call-next-method) (not (class-has-instance-slot-p superclass))))

(defclass data-object-class (patty-class)
  ())

(defgeneric slots-equality-expr (class &key var-a var-b))

(defmethod slots-equality-expr ((class patty-class) &key (var-a 'a) (var-b 'b))
  `(and ,@(loop for slot in (class-slots class)
		for reader = (slot-reader-name class (slot-definition-name slot))
		for equality = (slot-equality slot)
		collect `(,equality (,reader ,var-a) (,reader ,var-b)))))

(defmethod slots-equality-expr ((class data-unique-class)
				&key (var-a 'a) (var-b 'b))
  (declare (ignore var-a var-b))
  t)

(defgeneric define-equal-expr (class function-name))

(defmethod define-equal-expr ((class patty-class) function-name)
  `(lambda ()
    ;; NOTE: We use the class object directly instead of the class name,
    ;; since class may be anonymous. If I read the CLHS on defmethod correctly,
    ;; only a symbol would be allowed, but this works in sbcl, clisp and allegro.
    (defmethod ,function-name ((a ,class) (b ,class))
      ,(slots-equality-expr class :var-a 'a :var-b 'b))))

(defmethod finalize-class ((class patty-class))
  #+nil
  (format t "finalize-class: class ~S, slots ~S~%"
	  class (mapcar #'slot-equality slots))
  (loop for equal-function in (class-equal-functions class)
	do (funcall (compile nil
			     (define-equal-expr class equal-function)))))
