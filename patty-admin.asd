
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:cl-user)

(defpackage #:patty-admin-system (:use #:cl #:asdf))

(in-package #:patty-admin-system)

(defsystem #:patty-admin
  :name "patty-admin"
  :author "Stefan Lang <langstefan@gmx.at>"
  :licence "BSD"
  :description "Development and test tools for patty."
  :depends-on ("cl-ppcre")
  :serial t
  :components
  ((:file "patty-admin")))
