;;;; Uses the test program and prtest to test Parmenides

;;(load "/usr/pshell/lisp/test")
;;(load "/usr/pshell/parmenides/prtest.lisp")

(def-frame is-part-of (is-a (relation))
  combination-type first
  slots-inherited (value '*ALL*))
 
(def-frame truck ()
   color blue
   weight 10000
   material steel)
 
(def-frame door (is-part-of (truck))
  is-part-of truck
  width 3
  height 2)
 
(def-frame more (is-a door)
  width 4)

(make-truck 'truck1 :color 'red :weight 200)
(make-door 'door1 :is-part-of 'truck1 :width 5 :height 5)

(test-case get-value ('leg2a 'made-from) (bark mehogany))
(test-case get-value ('leg2a 'location) wall)
(test-case get-value ('leg3a 'made-from) (bark mehogany))
(test-case get-value ('leg3a 'location) on-floor)
(test-case get-value ('leg1 'made-from) (cherry mehogany))
(test-case get-value ('child1 'talks) yes)
(test-case get-value ('child1 'works) no)
(test-case get-value ('input :output) r)
(test-case get-value ('leg5 :location) on-floor)
(test-case get-value ('super-partf :super-part-of) (sf sub-partf))

(test-case get-slot ('leg1 'location) wall)
(test-case get-slot ('contains 'inverse-name) part-of)
(test-case get-slot ('super-part :super-part-of) (s1))
(test-case get-slot ('door1 :%class) door)
(test-case get-slot ('door1 :color) red)
(test-case get-slot ('more :width) 4)

(test-case local-p ('door1 :color) T)
(test-case local-p ('door1 :dfdf) NIL)
(test-case local-p ('leg1 :made-from) T)
(test-case local-p ('leg5 :location) NIL)

;;;(test-cases <fname> <cases>)

(defun test-pa ()
  (format T "Running Parmenides test cases...~%")
  (set-facet-demons 'input :input :value 'r)
  (test 'get-value)
  (test 'get-slot)
  (test 'local-p))
