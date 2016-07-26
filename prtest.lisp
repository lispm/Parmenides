;;; This file contains code which tests Parmenides.  It attempts to every
;;; user-level function, and tests most of its functionality.

(use-package "PARMENIDES")

(format T "Testing Parmenides.~%~%")

(def-frame living-thing (propagate t)
  :breathes (value 'yes)
  :eats regularly)

(def-frame person (is-a (living-thing) propagate t)
  :legs (value 2)
  :talks (value 'yes))
(setq p1 (make-person 'p1 :talks '(value no)))

(def-frame child (is-a (person) propagate t)
  :talks (value 'no)
  :works (value 'no))
(setq child1 (make-child 'child1 :talks '(value yes)))

(def-frame diversion ()
  :fun (value 'usually))

(def-frame baby (is-a (person diversion))
  :cries (value 'sometimes))
(setq b1 (make-baby 'baby1 :cries '(value always)))
;;; User relation testing

(def-frame part2 (is-a (pa:relation))
  :combination-type append
  :slots-inherited (value '((location first) made-from)))

(def-frame table (cache :*ALL*)
  :size big
  :location (value 'on-floor)
  :made-from (value '(mehogany)))

(def-frame leg1 (part2 (table))
  :location wall
  :made-from (value '(cherry))
  :weight 10)

(def-frame part-of (:is-a pa:relation :propagate nil)
  :combination-type :FIRST
  :slots-inherited (:value '((location :first) (made-from :append)))
  :has-inverses T
  :inverse-name contains)

(def-frame leg2 (part-of table)
  :part-of table)

(def-frame leg5 (part-of table) :a b)
(setq leg2 (make-leg2 'leg2a :location '(value wall)
      	   	      :made-from '(value (bark))))
(setq leg3 (make-leg2 'leg3a :part-of 'table
      	   	      :made-from '(value (bark))))

(def-frame input (propagate t)
  :under nil
  :status nil
  :input (:value '(input n) :pre-if-set '(change-my-output))
  :output (:value T :post-if-set '(change-successor-output)))

(defun change-my-output ()
  (format T "Entered change-my-output ok~%")
  (set-value pa:frame :output newval))

(def-frame sub-part-of (is-a (pa:relation))
  :has-inverses T
  :inverse-name super-part-of)

(def-frame super-part ()
  :super-part-of ())

(def-frame sub-part ()
  :sub-part-of ())

(def-frame super-partf ())

(def-frame sub-partf ()
  sub-part-of (value 'super-partf))

(make-sub-part 's1 :sub-part-of 'super-part)
(make-sub-partf 'sf :sub-part-of '(value super-partf))

;;;; Simple Code Testing facility.

(defvar *FUNCTIONS-TO-TEST* NIL)

;;; Example: (test-case (1+ 1) 2)
(defmacro test-case (form output)
  `(let ((fname (car ',form)))
     (pushnew fname *FUNCTIONS-TO-TEST*)
     (push (cons ',(cdr form) ',output)
	   (get fname :testlist))))

;;; Example: (test-cases 1+ ((1 . 2) (-1 . 0)))
(defmacro test-cases (fname cases)
  `(setf (getf ',fname :testlist) ',cases))

;;;It is assumed that the name of function, fname, has a :testlist property
;;;of dotted pairs of (data . expected-result).
(defun test (fname)
  (dolist (testcase (get fname :testlist))
    (format T "Testing ~A on ~A..." fname (car testcase))
    (let ((res (eval `(,fname ,@(car testcase)))))
      (cond ((not (equal res (cdr testcase)))
	     (cerror "Try next test case~%"
		     "~%oops, ~A returned ~A instead of expected ~A~%"
		     fname res (cdr testcase)))
	    (T (format T "~A, correct.~%" res))))))

;;; Top-level testing function.
(defun test-all ()
  (dolist (fname *FUNCTIONS-TO-TEST*)
    (test fname)))

(defun clear-tests ()
  (dolist (fname *FUNCTIONS-TO-TEST*)
    (remprop fname :testlist)))


;;; The testing code below used to be in testpa.lisp but has been merged
;;; into this file.

(def-frame is-part-of (:is-a (relation))
  :combination-type first
  :slots-inherited (value :*ALL*))
 
(def-frame truck ()
   :color blue
   :weight 10000
   :material steel)
 
(def-frame door (:is-part-of (truck))
  :is-part-of truck
  :width 3
  :height 2)
 
(def-frame more (:is-a door)
  :width 4)

(make-truck 'truck1 :color 'red :weight 200)
(make-door 'door1 :is-part-of 'truck1 :width 5 :height 5)
(set-facet-demons 'input :input :value 'r)

(test-case (get-value 'leg2a 'made-from) (bark mehogany))
(test-case (get-value 'leg2a 'location) wall)
(test-case (get-value 'leg3a 'made-from) (bark mehogany))
(test-case (get-value 'leg3a 'location) on-floor)
(test-case (get-value 'leg1 'made-from) (cherry mehogany))
(test-case (get-value 'child1 'talks) yes)
(test-case (get-value 'child1 'works) no)
(test-case (get-value 'input :output) r)
(test-case (get-value 'leg5 :location) on-floor)
(test-case (get-value 'super-partf :super-part-of) (sf sub-partf))

(test-case (get-slot 'leg1 'location) wall)
(test-case (get-slot 'contains 'inverse-name) part-of)
(test-case (get-slot 'super-part :super-part-of) (s1))
(test-case (get-slot 'door1 :%class) door)
(test-case (get-slot 'door1 :color) red)
(test-case (get-slot 'more :width) 4)

(test-case (local-p 'door1 :color) T)
(test-case (local-p 'door1 :dfdf) NIL)
(test-case (local-p 'leg1 :made-from) T)
(test-case (local-p 'leg5 :location) NIL)

(add-cslot 'door :size 'big)
(add-slot 'door :size-slot '(value 'big-slot))
(add-to-facet 'door :size-slot :value 'bigger)
(add-to-value 'door :size-slot 'biggest)
(add-to-slot 'door :weight 10)

(test-case (get-cslot 'door :size) big)
(test-case (get-value 'door :size-slot) (biggest bigger . big-slot))
(test-case (get-slot 'door :weight) (10 . 10000))
(test-case (pa-class-of 'door1) door)
(test-case (framep 'leg1) T)
(test-case (framep 'leg100) NIL)
(test-case (classp 'leg1) T)
(test-case (classp 'leg2a) NIL)
(test-case (classp 'leg100) NIL)
(test-case (slotp 'door :material) T)
(test-case (slotp 'door :wing) NIL)
(test-case (facetedp 'door :material) NIL)
(test-case (facetedp 'door :size-slot) T)
(test-case (frame-instance-p 'door1) T)
(test-case (frame-instance-p 'door) NIL)
(test-case (frame-instance-p 'door100) NIL)
(test-case (get-generic-value leg2 :location) wall)
(test-case (get-generic-value 'part-of :inverse-name) contains)
(test-case (get-immediate-facet 'leg2a :made-from :value) (bark mehogany))
(test-case (get-immediate-slot 'leg1 :location) wall)
(test-case (get-immediate-value 'leg2a :made-from) (bark mehogany))
(test-case (get-slot-names 'leg1) (:%class :location :made-from :weight))
(test-case (get-value 'leg2a :made-from) (bark mehogany))
(test-case (get-value-demons 'leg2a :made-from) (bark mehogany))
(test-case (immediate-isas 'person) (living-thing))
(test-case (instance-names-of 'person) (p1))
(test-case (inverse-isas 'person) (baby child))
(test-case (isa-instance leg3 'leg2) T)
(test-case (isa-instance leg3 'fooo) NIL)
(test-case (isa-p 'person 'living-thing) (living-thing))
(test-case (isa-p 'person 'nothing) NIL)
(test-case (isas 'baby) (person diversion living-thing))
(test-case (local-p 'baby :cries) T)
(test-case (local-p 'person :breathes) NIL)
(test-case (local-p 'person :legs) T)

(make-frame 'door 'd2)
(remove-frame 'd2)
(test-case (framep 'd100) NIL)
(make-frame 'door 'd2)
(make-frame 'door 'd3)
(make-frame 'door 'd4)
(make-frame 'door 'd5)
(set-facet 'd2 :size-slot :value 'newvalue)
(test-case (get-value 'd2 :size-slot) newvalue)
(set-facet-demons 'd5 :size-slot :value 'somevalue)
(test-case (get-value 'd5 :size-slot) somevalue)
(set-slot 'd2 :color 'purple)
(test-case (get-slot 'd2 :color) purple)
(add-cslot 'door :weight 'heavy)
(set-cslot 'door :weight 'light)
(test-case (get-cslot 'door :weight) light)

(set-value 'd3 :size-slot 'pink)
(test-case (get-value 'd3 :size-slot) pink)
(set-value-demons 'd4 :size-slot 'green)
(test-case (get-value 'd4 :size-slot) green)

(defparameter *size-slot-values* NIL)
(do-facets (name val :size-slot 'd4)
	   (push val *size-slot-values*))
(test-case (equal *size-slot-values* '(0 green)) T)
(test-case (isa-or-instance 'person 'living-thing) (living-thing))
(test-case (isa-or-instance (frame 'person) 'living-thing) (living-thing))
(test-case (save-frame 'door) NIL)	;; Just calling it tests it is defined
(test-case (pp-frame 'door) NIL)
(define-facet-getter door :size-slot :depth)
(define-facet-setter door :size-slot :depth)
(set-door-size-slot.depth (frame 'door) 1)
(test-case (door-size-slot.depth (frame 'door)) 1)
(test-case (define-facet-accessors door :size-slot :depth) door-size-slot.depth)

(test-all)
