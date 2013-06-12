
(in-package #:patty.test)

(deftestsuite test-defmethods (test-patty)
  ())

(addtest single-clause
  (ensure-same '(symbol-macrolet ()
		 (macrolet ()
		   (labels ()
		     (defmethod foo ((a string))
		       (print a)))))
	       (macroexpand-1 '(patty:defmethods foo
				(= ((a string))
				   (print a))))))

(addtest where-clause
  (ensure-same '(symbol-macrolet ()
		 (macrolet ()
		   (labels ((fun (x)
			      (1+ x)))
		     (defmethod foo ((a number))
		       (print (fun x))))))
	       (macroexpand-1 '(patty:defmethods foo
				(= ((a number))
				   (print (fun x)))
				(where fun (x) (1+ x))))))

(addtest syntax-clause
  (ensure-same '(symbol-macrolet ()
		 (macrolet ((plus-one (x)
			      `(1+ ,x)))
		   (labels ()
		     (defmethod foo ((a number))
		       (print (plus-one a))))))
	       (macroexpand-1 '(patty:defmethods foo
				(= ((a number))
				   (print (plus-one a)))
				(syntax plus-one (x) `(1+ ,x))))))

(addtest expr-clause
  (ensure-same '(symbol-macrolet ((1+a (1+ a)))
		 (macrolet ()
		   (labels ()
		     (defmethod foo ((a number))
		       (print 1+a)))))
	       (macroexpand-1 '(patty:defmethods foo
				(= ((a number))
				   (print 1+a))
				(expr 1+a (1+ a))))))

(addtest docstring
  (ensure-same '(symbol-macrolet ()
		 (macrolet ()
		   (labels ()
		     (defmethod foo ((a string))
		       "print the string a"
		       (print a)))))
	       (macroexpand-1 '(patty:defmethods foo
				(= ((a string))
				   "print the string a"
				   (print a))))))
