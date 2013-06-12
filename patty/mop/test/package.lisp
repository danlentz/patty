
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(defpackage #:patty.mop.test
  (:use #:cl #:lift #:patty.mop)
  (:import-from #:patty.test
		#:test-patty
		#:with-temp-package)
  (:import-from #:closer-mop
		#:class-finalized-p
		#:finalize-inheritance)
  (:export #:test-all))

(in-package #:patty.mop.test)

(deftestsuite test-mop (test-patty)
  ())
