
(in-package #:patty.mop.test)

(deftestsuite test-util (test-mop)
  ()
  (:setup (mapc #'finalize-inheritance
		(remove-if #'class-finalized-p
			   (mapcar #'find-class '(ca cb cc cd ce cf))))))

(defclass ca ()
  ())

(defclass cb ()
  ((b1)))

(defclass cc (ca)
  ((c1)))

(defclass cd (cc)
  ((d1)))

(defclass ce ()
  ((e1)
   (e2)))

(defclass cf ()
  ((f1 :allocation :class)))

(addtest class-has-instance-slot-p/no-slot
  (ensure (not (class-has-instance-slot-p (find-class 'ca)))))

(addtest class-has-instance-slot-p/one-slot
  (ensure (class-has-instance-slot-p (find-class 'cb))))

(addtest class-has-instance-slot-p/one-slot<no-slot
  (ensure (class-has-instance-slot-p (find-class 'cc))))

(addtest class-has-instance-slot-p/one-slot<one-slot
  (ensure (class-has-instance-slot-p (find-class 'cd))))

(addtest class-has-instance-slot-p/two-slots
  (ensure (class-has-instance-slot-p (find-class 'ce))))

(addtest class-has-instance-slot-p/one-class-slot
  (ensure (not (class-has-instance-slot-p (find-class 'cf)))))
