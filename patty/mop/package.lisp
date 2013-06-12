
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(defpackage #:patty.mop
  (:use #:cl #:sb-mop
	;; #:closer-mop
	#:moptilities)
  (:export #:singleton-class
	   #:reset-singleton-classes
	   #:abstract-class
	   #:define-abstract-class
	   #:patty-class
	   #:data-type-class
	   #:data-unique-class
	   #:data-object-class
	   #:patty-slot-definition
	   #:slot-equality
	   #:slot-reader-name
	   #:slot-writer-names
	   #:class-has-slot-writer-p
	   #:class-has-instance-slot-p)
  (:documentation "Please don't use other symbols than singleton-class,
abstract-class and define-abstract-class from this package. The rest is
currently for patty internal usage only."))
