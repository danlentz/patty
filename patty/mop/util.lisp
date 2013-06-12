
(in-package #:patty.mop)

(defun slot-reader-name (class slot-name)
  "Returns the name (as symbol) of one slot reader function for the
slot named slot-name in the given class or one of its superclasses.
Returns nil if there is no reader for a slot with the given name in
class or one of its superclasses."
  (loop for slot in (class-direct-slots class)
	when (and (eq slot-name (slot-definition-name slot))
		  (slot-definition-readers slot))
	do (return-from slot-reader-name (first (slot-definition-readers slot))))
  (loop for superclass in (class-direct-superclasses class)
	for reader-name = (slot-reader-name superclass slot-name)
	when reader-name
	do (return-from slot-reader-name reader-name))
  nil)

(defun slot-writer-names (class slot-name)
  "Returns a list of the names of all slot writer functions of all slots
of class and its superclasses. A name is either a symbol or a list whose
first element is the symbol SETF and whose second and last element another
symbol."
  (let ((writers nil))
    (loop for slot in (class-direct-slots class)
	  when (eq slot-name (slot-definition-name slot))
	  do (dolist (writer (slot-definition-writers slot))
	       (pushnew writer writers :test 'equal)))
    (loop for superclass in (class-direct-superclasses class)
	  do (dolist (writer (slot-writer-names superclass slot-name))
	       (pushnew writer writers :test 'equal)))
    writers))

(defun class-has-slot-writer-p (class)
  "Returns a generalized boolean indicating wheter class or one of its
superclasses has any slot with a slot writer or accessor."
  (loop for slot in (class-slots class)
	when (slot-writer-names class (slot-definition-name slot))
	do (return t)))

(defun class-has-instance-slot-p (class)
  "Returns a generalized boolean indicating wheter class or one of its
superclasses has any slot with instance allocation."
  (loop for slot in (class-slots class)
	when (eq :instance (slot-definition-allocation slot))
	do (return t)))
