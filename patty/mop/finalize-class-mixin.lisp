
(in-package #:patty.mop)

(defclass finalize-class-mixin (standard-class)
  ())

(defmethod validate-superclass ((class finalize-class-mixin)
				(superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass finalize-class-mixin))
  t)

(defgeneric finalize-class (class)
  (:method ((class class))
    "Noop, constantly returns nil."
    nil
    #+nil
    (format t "finalize-class ~S, slots: ~S~%" class (class-slots class))))

(defparameter *outermost-finalize-inheritance* t
  "Only for communication in finalize-inheritance. Don't touch!")

(defmethod finalize-inheritance :around ((class finalize-class-mixin))
  (declare (special *finalize-class-list*))
  #+nil(format t "finalize-inheritance ~S~%" class)
  (let ((res nil))
    (flet ((ensure-parents-finalized ()
	     (mapc #'finalize-inheritance
		   (remove-if #'class-finalized-p
			      (class-direct-superclasses class)))))
      (if *outermost-finalize-inheritance*
	  (let ((*outermost-finalize-inheritance* nil)
		(*finalize-class-list* (moptilities:subclasses class :proper? nil)))
	    (declare (special *finalize-class-list*))
	    (setf res (call-next-method))
	    (ensure-parents-finalized)
	    (mapc #'finalize-class
		  (remove-if-not #'class-finalized-p *finalize-class-list*)))
	  (progn
	    (setf *finalize-class-list*
		  (union *finalize-class-list*
			 (moptilities:subclasses class :proper? nil)))
	    (setf res (call-next-method))
	    (ensure-parents-finalized))))
    res))

#|
(defclass foo ()
  ((a :initarg :a :reader foo-a))
  (:metaclass finalize-class-mixin))

(defclass bar (foo)
  ((b :initarg :b :reader bar-b))
  (:metaclass finalize-class-mixin))

(make-instance 'bar :a 1 :b 2)
(make-instance 'foo :a 1)

(setf (find-class 'bar) nil)
(setf (find-class 'foo) nil)
|#
