;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - abstract-classes.lisp
;; Description	     - Abstract classes in CL
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Sun Dec 10 18:21:40 2000
;; Last Modified On  - Tue Apr 30 14:23:27 2002
;; Last Modified By  - Tim Bradshaw (tfb at lostwithiel)
;; Update Count	     - 14
;; Status	     - Unknown
;; 
;; $Id: abstract-classes.lisp,v 1.7 2002/04/30 13:24:50 tfb Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Abstract classes
;;;
;;; abstract-classes.lisp is copyright 2000-2001 by me, Tim Bradshaw,
;;; and may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

;;; This file has been modified by me, Stefan Lang, for the patty project.

(in-package :patty.mop)

(defclass abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes"))

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to make an instance of ~A which is an abstract class"
	 (class-name c)))

;;; The MOP requires this, but it's not clear that implementations do.
;;; VALIDATE-SUPERCLASS specifies when a superclass is suitable for a
;;; subclass. You have to be pretty specific, It's probably not in
;;; general safe to do what we do here.
;;;

(defmethod validate-superclass ((class abstract-class) 
				(superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS...
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass abstract-class))
  ;; ... and the other way around.
  t)


;;; I don't want to have to say ... (:metaclass abstract-class), but
;;; there is no easy hook into processing the options to DEFCLASS:
;;; ENSURE-CLASS-USING-CLASS, which would be the logical place to do
;;; this, is called with a class of NIL if there is no existing class,
;;; and so can't usefully be specialized.
;;;

(defmacro define-abstract-class (class supers slots &rest options)
  (when (assoc ':metaclass options)
    (error "Defining an abstract class with a metaclass?"))
  `(defclass ,class ,supers ,slots
	     ,@options
	     (:metaclass abstract-class)))

;;; Samples of abstract classes
#||
(define-abstract-class abstract-thing ()
  ((s :accessor thing-s)))

(defclass thing (abstract-thing)
  ((s :initform 1)))
||#

;;; Benchmarks: for ACL 6.0 there is no performance hit.
#||
(define-abstract-class ac () ())
(defclass ac-instantiable (ac) ())
(defclass nac () ())
(defclass nac-instantiable (nac) ())

(defun make-n-aci (n)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance 'ac-instantiable)))

(defun make-n-naci (n)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance 'nac-instantiable)))

(defun make-n-general (n cn)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance cn)))
||#
