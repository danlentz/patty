
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(defpackage #:patty
  (:documentation "Functional data structures on top of CLOS.")
  (:use #:cl #:alexandria #:patty.mop)
  (:export
    ;; patty builtin
    #:make
    #:defmethods
    #:defdata-type
    #:defdata-unique
    #:defdata-object
    ;; patty example (path api)
    #:path
    #:pathroot
    #:absolutep
    #:join-paths
    #:path=
    #:native-path
    #:unix-path
    #:relative-path
    #:absolute-path
    #:root-dir
    #:current-dir
    #:regular-path
    #:regular-absolute-path
    #:regular-relative-path))
