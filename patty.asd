
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:cl-user)

(defpackage #:patty-system (:use #:cl #:asdf))

(in-package #:patty-system)

(defsystem #:patty
  :name "patty"
  :author "Stefan Lang <langstefan@gmx.at>"
  :version "1.0.6"
  :licence "BSD"
  :description "Functional data types on top of CLOS."
  :depends-on ("alexandria" "closer-mop" "moptilities")
  :components
  ((:module "patty"
     :serial t
     :components
     ((:module "mop"
        :serial t
        :components
        ((:file "package")
          (:file "finalize-class-mixin")
          (:file "util")
          (:file "abstract-class")
          (:file "singleton-class")
          (:file "patty-class")))
       (:file "package")
       (:file"patty")
       (:file"path-class")
       ))))
