
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(defpackage #:patty-admin
  (:use #:cl)
  (:export #:extract-code
	   #:eval-doc-code))

(in-package #:patty-admin)

(defparameter *doc* (make-pathname :directory '(:relative "doc")
				   :name "patty" :type "txt"))

(defparameter *doc-code* (merge-pathnames (make-pathname :type "lisp") *doc*))

(defun rx-group (rx str)
  (multiple-value-bind (mstart mend group-starts group-ends)
      (cl-ppcre:scan rx str)
    (declare (ignore mend))
    (when mstart
      (subseq str (aref group-starts 0) (aref group-ends 0)))))

(defun extract-code ()
  "Extract code from file *doc* and save it in *doc-code*."
  (with-open-file (in *doc*)
    (with-open-file (out *doc-code* :direction :output :if-exists :supersede)
      (loop with state = :text
	    with indent = 0
	    for line = (read-line in nil nil)
	    while line do
	    (ecase state
	      (:text
	       (if (cl-ppcre:scan "^\\s+[a-zA-Z\\-]*>" line)
		   (setf state :repl)
		   (let ((ws (rx-group "^(\\s+)\\(" line)))
		     (when ws
		       (setf state :code)
		       (setf indent (length ws))))))
	      (:repl
	       (when (cl-ppcre:scan "^\\s*$" line)
		 (setf state :text)))
	      (:code
	       (when (cl-ppcre:scan "^\\S" line)
		 (setf state :text))))
	    (when (eq :code state)
	      (write-line (subseq line (if (< indent (length line))
					   indent 0))
			  out))))))

(defun eval-doc-code ()
  "Evaluates the Lisp expressions in file *doc-code*, in the current *package*."
  (with-open-file (in *doc-code*)
    (handler-case (loop (eval (read in)))
      (end-of-file () nil))))
