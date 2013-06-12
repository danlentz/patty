
;;;; This file is part of the patty project.
;;;; Copyright (c) 2007, Stefan Lang
;;;; For licencing conditions, read the LICENCE.txt file in the patty package.

(in-package #:patty.test)

(defparameter *ensure-doc-path*
  (merge-pathnames (make-pathname :directory '(:relative "patty" "test")
				  :name "ensure-doc" :type "lisp")
		   (asdf:component-pathname (asdf:find-system "patty"))))

(deftestsuite test-functional (test-patty)
  ())

(addtest doc-code
  (with-temp-package (pkg)
    (patty-admin:extract-code)
    (patty-admin:eval-doc-code)
    (import 'lift:ensure)
    (with-open-file (in *ensure-doc-path*)
      (handler-case (loop (eval (read in)))
	(end-of-file () nil)))))

(addtest object
  (with-temp-package (pkg)
    (use-package :patty)
    (eval (read-from-string "(progn
                               (defgeneric my= (a b))
                               (defdata-object my-object ()
                                 (:slots (my-slot :type string
                                                  :equality equalp))
                                 (:derive-equal my=)))"))
    (let ((cls (intern "MY-OBJECT" pkg))
	  (eqs (intern "MY=" pkg))
	  (sls (intern "MY-SLOT" pkg)))
      (let ((a (make-instance cls :my-slot "foo"))
	    (b (make-instance cls :my-slot "Foo")))
	(ensure (string= "foo" (funcall sls a)))
	(ensure (string= "Foo" (funcall sls b)))
	(ensure (funcall eqs a b)))
      (let ((a (make-instance cls :my-slot "foo"))
	    (b (make-instance cls :my-slot "bar")))
	(ensure (not (funcall eqs a b))))
      (ensure-error (make-instance cls))
      (ensure-error (make-instance cls :my-slot "foo" :my-other-slot "bar")))))

(addtest defdata-type
  (with-temp-package (pkg)
    (use-package :patty)
    (eval (read-from-string "(defdata-type my-type ())"))
    (ensure-error (make-instance (intern "MY-TYPE" pkg)))))

(addtest defdata-type-derive-equal
  (with-temp-package (pkg)
    (use-package :patty)
    (let ((expr (read-from-string "(defdata-type my-type ()
                                     (:derive-equal my=))")))
      (ensure-error (eval expr)))
    (ensure-error (find-class (intern "MY-TYPE")))))

(addtest defdata-object<standard-class
  (with-temp-package (pkg)
    (use-package :patty)
    (eval (read-from-string "(progn
                               (defgeneric my= (a b))
                               (defclass ca ()
                                 ((a :initarg :a :reader get-a)))
                               (defdata-object cb (ca)
                                 (:slots (get-b :initarg :b :equality char-equal))
                                 (:derive-equal my=)))"))
    (let ((cb (intern "CB"))
	  (my= (intern "MY=")))
      (let ((a (make-instance cb :a (1+ most-positive-fixnum) :b #\a))
	    (b (make-instance cb :a (1+ most-positive-fixnum) :b #\A)))
	(ensure (funcall my= a b)))
      (let ((a1 (make-instance cb :a (make-string 1 :initial-element #\a) :b #\a))
	    (b1 (make-instance cb :a (make-string 1 :initial-element #\a) :b #\a)))
	(ensure (not (funcall my= a1 b1)))))))

(addtest defdata-object<standard-class/writer
  (with-temp-package (pkg)
    (use-package :patty)
    (let ((expr (read-from-string "(progn
                                     (defclass ca ()
                                       ((a :initarg :a :accessor ca-a)))
                                     (defdata-object cb (ca)))")))
      (ensure-error (eval expr))
      (ensure-error (find-class (intern "CB"))))))

(addtest standard-class<defdata-object
  (with-temp-package (pkg)
    (use-package :patty)
    (eval (read-from-string "(progn
                               (defgeneric my= (a b))
                               (defdata-object ca ()
                                 (:slots (get-a :initarg :a :equality char-equal))
                                 (:derive-equal my=))
                               (defclass cb (ca)
                                 ((b :initarg :b :reader get-b))))"))
    (let ((cb (intern "CB"))
	  (get-a (intern "GET-A"))
	  (get-b (intern "GET-B")))
      (let ((obj (make-instance cb :a #\a :b "foo")))
	(ensure-same #\a (funcall get-a obj))
	(ensure-same "foo" (funcall get-b obj))))))

(addtest update-derive-equal
  (with-temp-package (pkg)
    (use-package :patty)
    (eval (read-from-string "(progn
                               (defgeneric my= (a b))
                               (defdata-object ca ()
                                 (:slots (get-a :equality string=)))
                               (defdata-object cb (ca)
                                 (:slots (get-b :equality string=))
                                 (:derive-equal my=)))"))
    (let ((my= (intern "MY="))
	  (cb (intern "CB")))
      (let ((a (make-instance cb :get-a "foo" :get-b "bar"))
	    (b (make-instance cb :get-a "foo" :get-b "bar")))
	(ensure (funcall my= a b)))
      (eval (read-from-string "(defdata-object ca ()
                                 (:slots (get-a :equality string=)
                                         (get-c :equality string=)))"))
      (let ((a1 (make-instance cb :get-a "foo" :get-b "bar" :get-c "baz"))
	    (b1 (make-instance cb :get-a "foo" :get-b "bar" :get-c "x")))
	(ensure (not (funcall my= a1 b1)))))))

(addtest redefine-class/old-object-slot
  (with-temp-package (pkg)
    (use-package :patty)
    (ensure (eql 1 (eval (read-from-string "(progn
                                              (defdata-object ca ()
                                                (:slots (sa :type number)))
                                              (defparameter *a* (make ca :sa 1))
                                              (sa *a*))"))))
    (ensure (eql 1 (eval (read-from-string "(progn
                                              (defdata-object ca ()
                                                (:slots (sa :type number)))
                                              (sa *a*))"))))))
