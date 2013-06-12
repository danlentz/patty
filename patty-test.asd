
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:cl-user)

(defpackage #:patty-test-system (:use #:cl #:asdf))

(in-package #:patty-test-system)

(defsystem #:patty-test
  :name "patty-test"
  :author "Stefan Lang <langstefan@gmx.at>"
  :licence "BSD"
  :description "Tests for patty."
  :depends-on ("patty" "patty-admin" "lift")
  :serial t
  :components
  ((:module "patty"
	    :serial t
	    :components
	    ((:module "test"
		      :serial t
		      :components
		      ((:file "package")
		       (:file "test-patty")
		       (:file "test-defmethods")))
	     (:module "mop"
		      :serial t
		      :components
		      ((:module "test"
				:serial t
				:components
				((:file "package")
				 (:file "test-util")))))))))
