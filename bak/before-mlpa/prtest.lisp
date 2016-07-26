;;; See also testpa.lisp

(format T "Testing Parmenides.~%")

(def-frame living-thing (propagate t)
  breathes (value 'yes)
  eats regularly)

(def-frame person (is-a (living-thing) propagate t)
  legs (value 2)
  talks (value 'yes))
(setq p1 (make-person 'p1 :talks '(value no)))

(def-frame child (is-a (person) propagate t)
  talks (value 'no)
  works (value 'no))
(setq child1 (make-child 'child1 :talks '(value yes)))

(def-frame diversion ()
  fun (value 'usually))

(def-frame baby (is-a (person diversion))
  cries (value 'sometimes))
(setq b1 (make-baby 'baby1 :cries '(value always)))
;;; User relation testing

(def-frame part2 (is-a (relation))
  combination-type append
  slots-inherited (value '((location first) made-from)))

(def-frame table (cache *ALL*)
  size big
  location (value 'on-floor)
  made-from (value '(mehogany)))

(def-frame leg1 (part2 (table))
  location wall
  made-from (value '(cherry))
  weight 10)

(def-frame part-of (:is-a relation :propagate nil)
  :combination-type :FIRST
  :slots-inherited (:value '((location :first) (made-from :append)))
  :has-inverses T
  :inverse-name contains)

(def-frame leg2 (part-of table)
  part-of table)

(def-frame leg5 (part-of table) a b)
(setq leg2 (make-leg2 'leg2a :location '(value wall)
      	   	      :made-from '(value (bark))))
(setq leg3 (make-leg2 'leg3a :part-of 'table
      	   	      :made-from '(value (bark))))

(def-frame input (propagate t)
  under nil
  status nil
  input (:value '(input n) :pre-if-set '(change-my-output))
  output (:value T :post-if-set '(change-successor-output)))

(defun change-my-output ()
  (format T "Entered change-my-output ok~%")
  (set-value frame :output newval))

(def-frame sub-part-of (is-a (relation))
  has-inverses T
  inverse-name super-part-of)

(def-frame super-part ()
  super-part-of ())

(def-frame sub-part ()
  sub-part-of ())

(def-frame super-partf ())

(def-frame sub-partf ()
  sub-part-of (value 'super-partf))

(make-sub-part 's1 :sub-part-of 'super-part)
(make-sub-partf 'sf :sub-part-of '(value super-partf))


;;; ---------------------------------------------------------------
#|
(def-frame is-part-of (:is-a relation :propagate nil)
  :combination-type first
  :slots-inherited *ALL*
  :has-inverses T
  :inverse-name has)

(def-frame truck ()
  :mat steel
  :weight 1000)

(def-frame door (is-part-of truck)
  :is-part-of truck
  :color red)

(def-frame handle (is-part-of door)
  is-part-of door
  weight nil
  length 20)

|#
