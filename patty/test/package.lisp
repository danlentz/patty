
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(defpackage #:patty.test
  (:use #:cl #:lift #:patty)
  (:export #:test-all))

(in-package #:patty.test)

(defun test-all ()
  "Run all patty tests, returning a lift test-result."
  (lift:run-tests :suite 'test-patty))

(deftestsuite test-patty ()
  ())

(defvar *temp-pkg-name-counter* 0)

(defun temp-pkg-name ()
  (format nil "%patty.test.temp-pkg-~A" (incf *temp-pkg-name-counter*)))

(defmacro with-temp-package ((&optional pkg-name) &body body)
  (let ((pkg (gensym))
	(pkg-name (or pkg-name (gensym))))
    `(let* ((,pkg-name (temp-pkg-name))
	    (,pkg (make-package ,pkg-name :use '(#:cl))))
      (unwind-protect (let ((*package* ,pkg))
			,@body)
	(delete-package ,pkg-name)))))
