;;; -*- Mode:Lisp; Syntax: Common-Lisp; Package:Common-Lisp-User -*-

;;; (setq *USER-PACKAGE* *PACKAGE*)
;;; (in-package 'parmenides)
;;; (use-package 'parmenides *USER-PACKAGE*)

;;; -------------------------------------------------------------------- ;;
;;; 	PARMENIDES -- A hierarchical class-based frame language.
;;;     Implemented in CommonLisp for RuleFermi 17-May-86.
;;; 	Copyright (c) 1986 Peter Shell.
;;; 	Some of the original Framekit functionality is preserved;
;;; 	however the main part has been re-designed and re-implemented, so
;;; 	this version will bear as much similarity to Loops as to Framekit.
;;; 	It is also influenced by the Slisp implementation of Defstruct,
;;; 	but it is more portable.
;;; 	This version is faster (but takes more space) because it
;;; 	represents frames with arrays, and so is able to define slot and
;;; 	facet accessor functions for each slot or facet.
;;; 	See the manual for complete details.
;;; 
;;; Source file:  [cmu-ri-ml]/usr/pshell/parmenides/parmenides.slisp
;;;  The type of inheritance is controlled by the variable:
;;;  !!inheritance-type documented below.
;;; --------------------------------------------------------------------
;;;                           Change log
;;;   23-Feb-87.  Changed def-frame so it no longer takes a <parents>
;;; argument but instead the parents are specified through the is-a cslot.
;;; Also, NIL now means facetless slots.
;;;   24-Jan-87. Added optional specification of inheritance type for slots
;;; specified in the slots-inherited slot of relations.  See the part-of
;;; relation for examples.
;;;   7-Jan-87. Now eval'ing default values at make time instead of def-frame
;;; time.  A side-effect of this is that default values are stored UN-evaled
;;; with the class, so get-facet will return the UN-evaled value for classes.
;;; Get-facet2 and get-value2 were thus added to make this transparent:
;;; if the frame accessed is a class, then evaluate the filler.
;;;   29-Nov-86.  Took out the extendable class facet.  Now everything is
;;; extendable.  This eliminates the need for special treatment of
;;; the extendable flag as well as having to pre-define all those svref fns.
;;;   27-Sep-86.  Added a setfable class facet.  Now Parmenides will define the
;;; methods only if setfable is T (by default NIL).
;;;   25-Sep-86.  Took out setf methods since setf using them is as
;;; slow as using set-facet or set-slot, and the expansion of them take up
;;; so much space since there are no pre-defined setting functions for facets,
;;; and pre-defining them would take up too much room.
;;;   7-SEP-86  PShell.  Made def-frame not define facet accessors and setf
;;; 	methods for other than the first facet, since it takes up so much
;;; 	space and time. Also took out setf and access functions for class
;;; 	facets.  Added make-frame, make-frame0 and make-slot.
;;;   17-MAY-86  PShell Created

;;; --------------------------------------------------------------------

;;; This file should be loaded before it is compiled.

;;; TO DO: make make-frame0 work with *ALL* for slots-inherited slot.
;;; 	   document optional specification of combo-type.
;;; Add an if-made facet to compute the value of a slot when the instance
;;;     is made.  The computed value would be stored in the value facet.
;;; Index-plist stuff should be replaced by something more efficient.

;;; Proclaim the following symbols as dynamically scoped so the demons
;;; can get the bindings when they fire...
(proclaim '(special framename slotname facetname frame snum facetnum newval
		    *APPEND-SLOTS* *THINGS-TO-EVAL* *AREF-SLOT-FN-PLIST*))

(defvar *DEFAULT* (list nil))	;;Unique default value, for make-frame-slot

(defun init-parmenides ()
  (setq *AREF-SLOT-FN-PLIST* nil)
  (format t "Parmenides 1.4~%"))


;;; Macros and things....

;;; Like concat but expands what it can at expansion time and otherwise wraps
;;; a symbol-name call to function calls.
(defmacro smash (&rest seqs)
    `(intern (concatenate 'string ,.(prepare-seqs seqs))))

(eval-when (load compile eval)
  (defun prepare-seqs (seqs)
    (mapcar #'prepare-seq seqs))

  (defun prepare-seq (seq)
    (cond ((numberp seq) (princ-to-string seq))
	  ((listp seq) `(princ-to-string ,seq))
	  ((not (stringp seq)) `(smart-symbol-name ,seq))
	  (T seq)))

  (defun smart-symbol-name (thing)
    (cond ((or (numberp thing) (listp thing))
	   (princ-to-string thing))
	  ((not (stringp thing)) (symbol-name thing))
	  (T thing)))

  ;;(defmacro putprop (obj val slot)
  ;;  `(setf (get ,obj ,slot) ,val))

  (if (not (fboundp 'memq))
      (defmacro memq (a l)
	`(member-if #'(lambda (x) (eq ,a x)) ,l)))

  (defmacro maybe-push (a list)
    `(and (not (memq ,a ,list))
	  (push ,a ,list))))

(defun property-names (plist)
  (do ((slots plist (cddr slots))
       (res nil (cons (car slots) res)))
       ((null slots) (nreverse res))))


;;; The lisps that this works under...
(eval-when (load eval compile)
  (defun kyoto-lisp-p ()
    (equal (lisp-implementation-type) "Kyoto Common Lisp on MV"))

  (defun spice-lisp-p ()
    (equal (lisp-implementation-type) "Spice Lisp"))

  (defun symbolics-lisp-p ()
    (equal (lisp-implementation-type) "Zetalisp"))

  (defun dec-lisp-p ()
    (equal (lisp-implementation-type) "VAX LISP")))

;;;Dec lisp wrongly evaluates printer names in defstruct.
#+DEC(dolist (pname '(pa-frame-printer))
       (set pname pname))

;;; A hack around the Kyoto carriage-return bug:
#+kcl (defvar *FORMAT* (symbol-function 'format))
#+kcl (defun format (stream string &rest vars)
	(terpri)
	(apply *FORMAT* `(,stream ,string ,@vars)))

(eval-when (eval compile load)
 (defvar *AREF-FN-NAMES*
  '(%aref-get-0 %aref-get-1 %aref-get-2 %aref-get-3
		%aref-get-4 %aref-get-5 %aref-get-6 %aref-get-7
		%aref-get-8 %aref-get-9  %aref-get-10
		%aref-get-11 %aref-get-12 %aref-get-13
		%aref-get-14 %aref-get-15))

 (defvar *AREF-FACET1-FN-NAMES*
  '(%aref-value-get-0 %aref-value-get-1 %aref-value-get-2 %aref-value-get-3
    %aref-value-get-4 %aref-value-get-5 %aref-value-get-6 %aref-value-get-7
    %aref-value-get-8 %aref-value-get-9  %aref-value-get-10
    %aref-value-get-11 %aref-value-get-12 %aref-value-get-13
    %aref-value-get-14 %aref-value-get-15)))

(proclaim `(inline ,@*AREF-FN-NAMES* ,@*AREF-FACET1-FN-NAMES*))

;;; To save space and time.  Since lots of facet accessor functions do the
;;; same thing, you might as well make them be eq to same canonical aref
;;; accessor. (like lisp::%get).
(defun %aref-get-0 (frame) (aref (the array frame) 0))
(defun %aref-get-1 (frame) (aref (the array frame) 1))
(defun %aref-get-2 (frame) (aref (the array frame) 2))
(defun %aref-get-3 (frame) (aref (the array frame) 3))
(defun %aref-get-4 (frame) (aref (the array frame) 4))
(defun %aref-get-5 (frame) (aref (the array frame) 5))
(defun %aref-get-6 (frame) (aref (the array frame) 6))
(defun %aref-get-7 (frame) (aref (the array frame) 7))
(defun %aref-get-8 (frame) (aref (the array frame) 8))
(defun %aref-get-9 (frame) (aref (the array frame) 9))
(defun %aref-get-10 (frame) (aref (the array frame) 10))
(defun %aref-get-11 (frame) (aref (the array frame) 11))
(defun %aref-get-12 (frame) (aref (the array frame) 12))
(defun %aref-get-13 (frame) (aref (the array frame) 13))
(defun %aref-get-14 (frame) (aref (the array frame) 14))
(defun %aref-get-15 (frame) (aref (the array frame) 15))

;;; Predefine the facet access function which takes the 0th facet from a
;;; slot (this is always the value facet for Rulekit).
(defun %aref-value-get-0 (frame) (aref (the array (aref (the array frame) 0)) 0))
(defun %aref-value-get-1 (frame) (aref (the array (aref (the array frame) 1)) 0))
(defun %aref-value-get-2 (frame) (aref (the array (aref (the array frame) 2)) 0))
(defun %aref-value-get-3 (frame) (aref (the array (aref (the array frame) 3)) 0))
(defun %aref-value-get-4 (frame) (aref (the array (aref (the array frame) 4)) 0))
(defun %aref-value-get-5 (frame) (aref (the array (aref (the array frame) 5)) 0))
(defun %aref-value-get-6 (frame) (aref (the array (aref (the array frame) 6)) 0))
(defun %aref-value-get-7 (frame) (aref (the array (aref (the array frame) 7)) 0))
(defun %aref-value-get-8 (frame) (aref (the array (aref (the array frame) 8)) 0))
(defun %aref-value-get-9 (frame) (aref (the array (aref (the array frame) 9)) 0))
(defun %aref-value-get-10 (frame) (aref (the array (aref (the array frame) 10)) 0))
(defun %aref-value-get-11 (frame) (aref (the array (aref (the array frame) 11)) 0))
(defun %aref-value-get-12 (frame) (aref (the array (aref (the array frame) 12)) 0))
(defun %aref-value-get-13 (frame) (aref (the array (aref (the array frame) 13)) 0))
(defun %aref-value-get-14 (frame) (aref (the array (aref (the array frame) 14)) 0))
(defun %aref-value-get-15 (frame) (aref (the array (aref (the array frame) 15)) 0))


;;; (aref *AREF-FN-MAP* N) returns the function object which is the aref
;;; accessor for the Nth slot.
(eval-when (eval compile load)
  (defvar *AREF-FN-MAP*
    (make-array 16
		:initial-contents *AREF-FN-NAMES*))
  (defvar *AREF-FACET1-FN-MAP*
    (make-array 16
		:initial-contents *AREF-FACET1-FN-NAMES*))
  (defvar *aref-slot-fn-plist* nil))

;;; Structure for a Parmenides frame. This has not been a recording.
(defstruct (pa-frame (:print-function pa-frame-printer))
  propagatep	  ;;T iff it's a propagating kind of frame.
  index-plist	  ;;nested plist associating slot, facet names with array indices.
  slots		  ;;list of all cached slots, facets & default vals.
  c-slots	  ;;plist of slots which are only stored with the class
  numslots	  ;;used when the frame is adjustable
  name		  ;;name of the frame
  snames	  ;;list of slot names
  rel-slots	  ;;list of slot names which are relations
 )

(defvar *MARKED-CLASSES* NIL)	 ;;For marker propagation

(defvar !!inheritance-type 'dfs) ;; defaults to depth-first search
				 ;; dfs = depth first search (stopping
				 ;; on first value list found)
(defvar !!inheritance-link 'is-a)


; -------------------------------------------------------------------
; 		Basic retrieval functions:
; -------------------------------------------------------------------

(defvar !!if-accessed 'nil)

;;; For predicates which return 2 values.  For the first member of seq such
;;; that the second value of pred is not nil, this returns the two values
;;; returned by that pred.  If it gets to the end of the sequence, it returns
;;; (values nil nil).
(defmacro some2 (pred seq)
  (let ((varname (gentemp "VAR")))
    `(dolist (,varname ,seq (values nil nil))
       (multiple-value-bind (first second)
			    (funcall ,pred ,varname)
	 (if second (return (values first second)))))))

;;; Returns (values value foundp)
(defun get-facet (framename slotname facetname)
  (let* ((frame (name-to-frame framename))
	 (frame-type (name-to-frame-type framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec))
	 (facetnum (getf (cdr slot-spec) facetname)))
    (cond ((and snum facetnum)
	   (values (aref (aref frame snum) facetnum) T))
	  ((null (isas framename)) (values NIL NIL))
	  (t (some2 #'(lambda (parent)	;;depth-first inheritance
			(get-facet parent slotname facetname))
		    (isas framename))))))

(defun get-instance-facet (frame slotname facetname)
  (let* ((framename (frame-class frame))
	 (frame-type (name-to-frame-type framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec))
	 (facetnum (getf (cdr slot-spec) facetname)))
    (cond ((and snum facetnum)
	   (values (aref (aref frame snum) facetnum) T))
	  ((null (isas framename)) (values NIL NIL))
	  (t (some2 #'(lambda (parent)	;;depth-first inheritance
			(get-facet parent slotname facetname))
		    (isas framename))))))

(defmacro get-value (frame slot) ;get value facet
  `(get-facet ,frame ,slot 'value))

(defun get-facet-demons (framename sname facetname)
   (let* ((frame (name-to-frame framename))
	 (frame-type (name-to-frame-type framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index sname))
	 (snum (car slot-spec))
	 (facetnum (getf (cdr slot-spec) facetname))
	 (val (aref (aref frame snum) facetnum)))
    (if (not val)
	(let* ((demonfacetnum (getf (cdr slot-spec) 'if-needed))
	       (local-demon (and demonfacetnum
				 (aref (aref frame snum) demonfacetnum)))
	       (val (and local-demon
			 (let ((framename framename)	;;local bindings for demon
			       (slotname sname)
			       (facetname facetname))
			   (eval local-demon)))))
	  (if (not val)
	      (let* ((c-slots (pa-frame-c-slots frame-type))
		     (class-demon (getf c-slots 'if-needed))
		     (val (and class-demon
			       (let ((framename framename)
				     (slotname sname)
				     (facetname facetname)
				     (frame frame))
				 (eval class-demon)))))
		val)
	      val))
	val)))

(defsetf get-facet set-facet)	;;So you can do things like push on (get-facet ..)
(defsetf get-facet-demons set-facet-demons)
(defsetf get-value set-value)

(defun local-p (framename slotname)
  (zerop (get-facet framename slotname 'depth)))

(defun faceted-p2 (fi-plist sname)
  (let ((res (cdr (getf fi-plist sname))))
    (and res (consp res))))

(defmacro facetedp (class sname)
  `(faceted-p2 (pa-frame-index-plist (name-to-frame-type ,class)) ,sname))

;;; Returns the data structure associated with the particular slot/frame pair.
;;; If the slot has no facet, then it will be the value; else it will be a
;;; data structure which contains all the facets and values.
;;; Returns NIL if no such slot exists.
(defun get-slot (framename slotname)
  (let* ((frame (name-to-frame framename))
	 (frame-type (name-to-frame-type framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec)))
    (cond (snum (aref frame snum))
	  (t (some #'(lambda (parent) (get-slot parent slotname))
		   (isas framename))))))	;;depth-first inheritance.

(defun get-slot-num (class fi-plist sname)
  (let* ((frame (name-to-frame class))
	 (slot-spec (getf fi-plist sname))
	 (snum (car slot-spec)))
    snum))

;;; Returns (values <slotnum> <facetnum>)
(defun pa-get-snf-nums (classname sname facetname)
  (let* ((frame-type (name-to-frame-type classname))
	 (slot-spec (getf (pa-frame-index-plist frame-type) sname))
	 (snum (car slot-spec))
	 (facetnum (getf (cdr slot-spec) facetname)))
	(values snum facetnum)))

(defun get-instance-slot (frame slotname)
  (let* ((frame-type (name-to-frame-type (frame-class frame)))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec)))
    (and snum (aref frame snum))))

(defun get-cslot (framename slotname) ; "get-slot" w/o
  (let* ((frame-type (name-to-frame-type framename))
	 (slot-plist (pa-frame-c-slots frame-type)))
    (getf slot-plist slotname)))

;;; -----------------------------------------------------------------------
;;; 		Basic storage functions:
;;; -----------------------------------------------------------------------

(defun add-to-facet (framename slotname facetname filler) ;Adds filler to facet
  (push filler (get-facet framename slotname facetname)))

(defun add-to-facet-demons (framename slotname facetname filler) ;Adds w/demons
  (push filler (get-facet-demons framename slotname facetname)))

(defun add-to-value (frame slot filler) ; Adds filler to the value facet
  (push filler (get-facet frame slot 'value)))

;;; Set-facet

;;; Since the user is setting a facet, it is assumed that the given slot has
;;; facets.  Every slot that has facets has a value and depth facet.

;;; Works for both classes and instances.  Propagates the value down is-a and
;;; instance links if framename is a class.
(defun set-facet (framename slotname facetname newval)
  (init-propagate)
  (set-facet* framename slotname facetname newval 0 NIL)
  newval)

(defun set-value (framename slotname newval)
  (set-facet framename slotname 'value newval))

(defun set-facet-demons (framename slotname facetname newval)
  (init-propagate)
  (set-facet* framename slotname facetname newval 0 :demons)
  newval)

(defun set-instance-facet (frame slotname facetname newval)
  (let* ((classname (frame-class frame))
	 (frame-type (name-to-frame-type classname))
	 (depth 0)
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec))
	 (fplist (cdr slot-spec))
	 (facetnum (getf fplist facetname))
	 (dfacetnum (getf fplist 'depth)))
    (modify-facet-plist frame-type slotname facetname newval)
    (setf (aref (aref frame snum) facetnum) newval)
    (when (pa-frame-propagatep frame-type)
      (modify-facet-plist frame-type slotname 'depth depth)
      (setf (aref (aref frame snum) dfacetnum) depth))
    newval))

;;; Added 6-Sep-86
(defun set-instance-slot (frame slotname newval)
  (let* ((classname (frame-class frame))
	 (frame-type (name-to-frame-type classname))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec)))
    (setf (aref frame snum) newval)
    newval))

(defun set-facet* (framename slotname facetname newval depth demonsp)
  (let* ((frame-type (name-to-frame-type framename))
	 (frame (name-to-frame framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec))
	 (fplist (cdr slot-spec))
	 (facetnum (getf fplist facetname))
	 (postfacetnum (getf fplist 'post-if-set))
	 (prefacetnum (getf fplist 'pre-if-set))
	 (dfacetnum (getf fplist 'depth)))
    (modify-facet-plist frame-type slotname facetname newval)
    (and (pa-frame-propagatep frame-type)
	 (modify-facet-plist frame-type slotname 'depth depth))
    (set-class&instances-facet frame framename slotname facetname snum
			       facetnum newval depth dfacetnum
			       prefacetnum postfacetnum demonsp)
    (propagate-down framename slotname facetname newval (1+ depth) demonsp)
    newval))

;;; Instances have the property that all of their slot nums and facet nums are
;;; the same as their class, since they are an instance of only one class.
;;; Algorithm:
;;; (1) fire pre-set demons if demonsp is true and there are any.
;;; (2) set the desired facet in the frame
;;; (3) fire post-set demons if demonsp is true and there are any.
;;; (4) do the same for any/all instances
(defun set-class&instances-facet (frame framename sname fname snum fnum newval
				  depth dfnum prefacetnum postfacetnum demonsp)
  (when demonsp
    (if prefacetnum	;;if there is an if-added demon on the slot
	(let ((demon (aref (aref frame snum) prefacetnum))
	      (framename framename)
	      (slotname sname)
	      (facetname fname))
	  (eval demon)))
    (let ((cdemon (and framename (get-cslot framename 'pre-if-set))))
      (if cdemon
	  (let ((framename framename)
		(slotname sname)
		(facetname fname))
	    (eval cdemon)))))
  (setf (aref (aref frame snum) dfnum) depth)
  (setf (aref (aref frame snum) fnum) newval)
  (when demonsp
    (if postfacetnum	;;if there is an if-added demon on the slot
	(let ((demon (aref (aref frame snum) postfacetnum))
	      (framename framename)
	      (slotname sname)
	      (facetname fname))
	  (eval demon)))
    (let ((cdemon (and framename (get-cslot framename 'post-if-set))))
      (if cdemon
	  (let ((framename framename)
		(slotname sname)
		(facetname fname))
	    (eval cdemon)))))
  (let ((depth (1+ depth)))	;;instances are 1 away from the class.
    (dolist (instance (instances-of framename))
      (if (>= (aref (aref instance snum) dfnum) depth)
	  (set-class&instances-facet instance nil sname fname snum fnum
				     newval depth dfnum prefacetnum
				     postfacetnum demonsp)))))

(defun propagate-down (classname slotname facetname newval depth demonsp)
  (ecase !!inheritance-type
;;    (bfs (propagate-down-bfs classname slotname facetname newval depth demonsp))
    (dfs (propagate-down-dfs classname slotname facetname newval depth demonsp))))

(defun propagate-down-dfs (classname slotname facetname newval depth demonsp)
  (dolist (descendent (inverse-isas classname))
    (when (and (pa-frame-propagatep (name-to-frame-type descendent))
	       (>= (get-facet descendent slotname 'depth) depth)
	       (not (marked-p descendent)))
      (mark-classname descendent)
      (set-facet* descendent slotname facetname newval depth demonsp))))

(defun modify-facet-plist (classtruct slotname facetname newval)
  (setf (getf (getf (pa-frame-slots classtruct) slotname) facetname) newval))

(defun modify-slot-plist (classtruct slotname newval)
  (setf (getf (pa-frame-slots classtruct) slotname) newval))

;;; Can't propagate down since you don't know the depth.
;;; Can't fire demons because there are no facets to store demons on.
(defun set-slot (framename slotname newval)
  (let* ((frame-type (name-to-frame-type framename))
	 (frame (name-to-frame framename))
	 (slot-index (pa-frame-index-plist frame-type))
	 (slot-spec (getf slot-index slotname))
	 (snum (car slot-spec)))
    (modify-slot-plist frame-type slotname newval)
    (setf (aref frame snum) newval)
    newval))

;;; Utilities for set-facet...

(defun mark-classname (name)
  (putprop name t 'marked)
  (push name *MARKED-CLASSES*))

(defun marked-p (name)
  (get name 'marked))

(defun classp (framename)
  (get framename 'classp))

(defun init-propagate ()
  (dolist (class *MARKED-CLASSES*)
    (putprop class nil 'marked))
  (setq *MARKED-CLASSES* nil))

(defun num-instances-of-class (classname)
  (length (get classname 'instances)))


;;; Modify & Remove-frame

;;; Same syntax as make but don't make a new copy - just set the slots
;;; indicated in the 'newslots' plist.
(defun modify-frame (frame newslots)
  (let* ((classname (frame-class frame))
	 (frame-type (name-to-frame-type classname))
	 (slot-index (pa-frame-index-plist frame-type)))
    (do ((slot newslots (cddr slot)))
	((null slot) T)
      (let* ((slot-spec (getf slot-index (keyword-to-user (car slot))))
	     (snum (car slot-spec)))
	(cond ((consp (cdr slot-spec))	;;That means it's faceted
	       (do ((facet (cadr slot) (cddr facet)))
		   ((null facet) T)
		 (let ((facetnum (getf (cdr slot-spec) (car facet))))
		   (setf (aref (aref frame snum) facetnum) (cadr facet)))))
	      (T
	       (setf (aref frame snum) (cadr slot)))))))
  frame)

(defun remove-frame (frame)
  (let ((class (frame-class frame)))
    (setf (get class 'instance-names)
	  (delete frame (get class 'instance-names)
		  :test #'(lambda (frame fname)
			    (eq (name-to-frame fname) frame))))
    (setf (get class 'instances)
	  (delete frame (get class 'instances)))))

;;; Copy-Frame
(defun copy-frame (frame)
  (if (adjustable-array-p frame)
      (copy-adjustable-array frame)
      (copy-nested-structure frame)))

(defun copy-adjustable-array (frame)
  (let* ((numslots (pa-frame-numslots (name-to-frame-type
					       (frame-class frame))))
	 (newframe (make-array numslots
			       :adjustable t)))
    (copy-array frame newframe numslots)))

(defun copy-nested-structure (frame)
  (let* ((numslots (pa-frame-numslots (name-to-frame-type
				    (frame-class frame))))
	 (newframe (make-array numslots)))
    (copy-array frame newframe numslots)))

(defun copy-array (a1 a2 numslots)
  (dotimes (i numslots)
    (setf (aref a2 i)
	  (copy-generic-thing (aref a1 i))))
  a2)

(defun copy-vector (a)
  (let* ((length (car (array-dimensions a)))
	 (newv (make-array length :adjustable (adjustable-array-p a))))
    (copy-array a newv length)))

;;; array, symbol or list
(defun copy-generic-thing (thing)
  (cond ((arrayp thing)
	 (copy-vector thing))
	((consp thing)
	 (copy-list thing))
	(thing)))

; ---------------------------------------------------------------------
; 	Basic top-level frame-definition functions
; ---------------------------------------------------------------------

(defun pa-frame-printer (frame stream depth)
  (declare (ignore depth))
  (format stream "Frame ~S:~%" (pa-frame-name frame))
  (format stream "Slots: ~S~%" (pa-frame-slots frame))
)

(defun pp-frame-name (framename &optional (stream *standard-output*))
  (pp-frame (name-to-frame framename) stream))

;;; Pretty-Prints frame instances (arrays)
;;; frame must be an array, but not necessarily a frame
(defun pp-frame (frame &optional (stream *STANDARD-OUTPUT*))
  (if (null frame)
      (format stream "No such frame.~%")
      (let ((snames (get-slot-names (frame-class frame))))
	(if (not snames)		;;then it's not a parmenides frame
	    (princ frame stream)
	    (pp-frame* frame snames stream)))))

;;; frame must be a frame.
(defun pp-frame* (frame snames stream)
;;  (format stream "~S:~%" (frame-class frame))
  (dolist (sname snames)
    (format stream "~A: ~S~%" sname (get-generic-value (keyword-to-user sname)
						       frame))))

;;; Return either the value facet if the slot named sname has facets, or
;;; the slot itself if it doesn't.
(defun get-generic-value (sname frame)
  (let ((slot (get-instance-slot frame sname)))
    (if (arrayp slot)
	(list 'value (aref slot 0))
	slot)))

(defun get-atomic-value (sname frame)
  (let ((slot (get-instance-slot frame sname)))
    (if (arrayp slot)
	(aref slot 0)
	slot)))

;;; Parmenides-eval (sort of).  Instead of evaling things, it puts them into
;;; a list so that def-frame can return this list of things to be evaled.
(defun pa-eval (exp)
  (push exp *THINGS-TO-EVAL*))

;;; Top-level, main frame-definition function.  Returns the instance of the frame
;;; made by filling it in with the default values.
(eval-when (eval load compile)
 (defmacro def-frame (name cslots &rest slots)
  (setq *THINGS-TO-EVAL* nil)
  (let* ((parents (check-parents (getf cslots 'is-a)))
	 (frame-cslots (find-frame-cslots parents))
	 (parent-cplist (combine-slots frame-cslots))
	 (local-cached (getf cslots 'cache))
	 (full-cplist (plist-union-no-propagate (delete-slot 'is-a cslots)
						parent-cplist))
	 (propagatep (getf full-cplist 'propagate T))	  ;;default is T
	 (setfable (getf full-cplist 'setfable NIL))
	 (other-related-frames (find-relation-frames full-cplist))
	 (slots (add-class slots name))
	 (frame-slots (find-frame-slots parents))
	 (full-iplist (plist-union slots frame-slots propagatep
				   other-related-frames))
	 (iall-slot-namesk (property-names full-iplist))
	 (index-plist (make-index-plist full-iplist full-cplist))
	 (theframe
	  (make-pa-frame
	   :propagatep propagatep
	   :numslots (length iall-slot-namesk)
	   :index-plist index-plist
	   :slots full-iplist
	   :c-slots full-cplist
	   :name name
	   :snames (mapcar #'current-to-keyword iall-slot-namesk)  ;;Inefficient!
	   :rel-slots (find-rel-slots full-iplist))))
    (setf (getf full-cplist 'cache)
	  (combine-cache-slots local-cached iall-slot-namesk frame-cslots parents))
    (putprop name theframe :frame-type)
    (write-accessor-functions theframe T name full-iplist setfable propagatep)
    (write-maker-function name)
    `(progn
      (announce-define ',name)
      (setf (get ',name 'instances) nil)
      (putprop ',name ',theframe :frame-type)
      (putprop ',name (make-default-frame ',name) :frame)
      (putprop ',name t 'classp)
      (compute-isas ',name ',parents)
      (compute-inverse-isas ',name ',parents)
      ,.*THINGS-TO-EVAL* ',name))))

(defun announce-define (classname)
  (cond ((frame-class-p classname)
	 (format T "Re-defining class ~A~%" classname))
	(T (format T "Defining class ~A~%" classname))))

;;; Filter out parent names which aren't classes.
(defun check-parents (fnames)
  (delete-if #'(lambda (fname)
		 (when (not (classp fname))
		   (format t "?Parent ~A is not a class, ignoring.~%" fname)
		   t))
	     fnames))

(defun write-maker-function (name)
  (pa-eval
   `(defun ,(smash "MAKE-" name) (iname &rest plist)
      (make-frame0 ',name iname plist))))

;;; Non-macro version of def-frame.
(defun def-frame* (name cplist slots)
  (eval `(def-frame ,name ,cplist ,@slots)))

(defun eval-plist (plist)
  (let ((pl (copy-list plist)))
    (do ((thunk pl (cddr thunk)))
	((null thunk) pl)
      (setf (cadr thunk) (eval (cadr thunk))))))

;;; Added 20-July-86.  Returns a list of the slots in plist which are
;;; appending relations.
(defun find-rel-slots (plist)
  (do ((relations plist (cddr relations))
       (res nil
	    (if (frame-class-p (car relations))
		(cons (current-to-keyword (car relations)) res)
		res)))
      ((null relations) res)))

;;; Added 14-July-86.  From a plist, returns a plist of (relation framename),
;;; the relations naming frames which are relations.
(defun find-relation-frames (plist)
  (do ((plist plist (cddr plist))
       (res nil (if (frame-class-p (car plist))
		    (cons (cadr plist) (cons (car plist) res))
		    res)))
      ((null plist) (nreverse res))))

(defun compute-isas (name parents)
  (putprop name (all-grandparents parents) 'isas))

;;; Return a list of all the parents of all the given parents, and the parents
;;; themselves, eliminating redundancies.
(defun all-grandparents (parents)
  (do ((parents parents (cdr parents))
       (res (copy-list parents)
	    (ordered-union res (get (car parents) 'isas))))
      ((null parents) res)))

(defun compute-inverse-isas (name parents)
  (dolist (parent parents)
    (if (not (memq name (get parent 'inverse-isas)))
	(push name (get parent 'inverse-isas)))))

;;; Adds the given name as a default of the %class to the slots list of a def-frame
(defun add-class (slots name)
  (append `(%class ,name) slots))

;;; The name of the class is always kept in the 0th position by Parmenides.
(defun frame-class (frame)
  (aref frame 0))

;;; Given the class name return an ordered list classes that it is-a.
(defun isas (classname)
  (get classname 'isas))

(defun inverse-isas (classname)
  (get classname 'inverse-isas))

;;; Returns T iff the given object is an instance of a frame.
(defun frame-p (frame)
  (let ((class (frame-class frame)))
    (and (symbolp class)
	 (frame-class-p class))))

;;; Return t iff the instance frame is an instance of classname, though not
;;; necessarily an immediate instance.
(defun isa-instance (frame classname)
 (and (frame-p frame)
      (frame-class-p classname)
      (or (eq classname (frame-class frame))
	  (memq classname (isas (frame-class frame))))))

(defun instance-names-of (framename)
  (get framename 'instance-names))

(defun instances-of (framename)
  (get framename 'instances))


;;; The maker functions.  (revised 9-Sep-86)

;;; General slot making function.  A little slower than defining a make
;;; function for each frame/slot pair, but saves lots of space,
;;; and makes compiling frame files much faster.  Added 7-Sep-86.
;;; Revised again 13-Feb-87 for speed.
(defun make-frame-slot (class sname fillers)
  (let* ((newframe (copy-seq (get-slot class sname))))
    (if fillers
	(do ((slots (getf (pa-frame-slots (name-to-frame-type class)) sname)
		    (cddr slots))
	     (fnum 0 (1+ fnum)))
	    ((null slots))
 	  (let ((filler (getf fillers (car slots) *DEFAULT*)))
	    (if (not (eq filler *DEFAULT*))
		(setf (aref newframe fnum) filler)))))
    newframe))

;;; Makes a default faceted slot (array) given fillers (plist of facets).
;;; Split off from make-frame-slot 14-Feb-87
(defun make-default-frame-slot (fillers)
  (let* ((newslot (make-array (/ (length fillers) 2))))
    (do ((facet fillers (cddr facet))
	 (fnum 0 (1+ fnum)))
	((null facet) newslot)
      (setf (aref newslot fnum) (cadr facet)))))

;;; General frame-making function.
(defun make-frame (class name &rest plist)
  (make-frame0 class name plist))

;;;  Plist is a list in this case.
(defun make-frame0 (class name plist)
  (declare (special plist class))
  (if (eq class name)
      (cerror "Don't make an instance."
	      "Sorry, the instance must not have the same name as the class, ~A"
	      class)
   (let* ((ftype (name-to-frame-type class))
	 (default-frame (name-to-frame class))
	 (newframe (make-array (pa-frame-numslots ftype)))
	 (fi-alist (pa-frame-index-plist ftype))
	 (relations (pa-frame-rel-slots ftype))
	 af-alist)	;;usname = non-keyword sname.
    (declare (special fi-alist af-alist default-frame newframe))
    (flet ((do-slot (sname)
      (let* ((usname (keyword-to-user sname))
	     (snum (get-slot-num class fi-alist usname))
	     (sval (getf plist sname *DEFAULT*)))
	(cond ((not (faceted-p2 fi-alist usname))
	       (if (eq sval *DEFAULT*)			     ;;if not provided then
		   (setq sval (aref default-frame snum))))   ;;use default
	      (t (setq sval (generic-inherit usname sval af-alist class
					     (pa-frame-propagatep ftype)))
		 (cond ((eq sval *DEFAULT*)
			(setq sval (copy-vector (aref default-frame snum)))
			(if (pa-frame-propagatep ftype)
			    (incf (aref sval 1)))))))
	(setf (aref newframe snum) sval)
	sval)))
      (dolist (rel-slot relations)
	(push (create-sf-entry rel-slot (do-slot rel-slot)) af-alist))
      (dolist (sname (pa-frame-snames ftype))
	(if (not (memq sname relations))
	    (do-slot sname))))
    (when name
      (putprop name newframe :frame)
      (putprop name ftype :frame-type)
      (putprop name (isas class) 'isas)
      (maybe-push name (get class 'instance-names)))
    (push newframe (get class 'instances))
    newframe)))

(defun generic-inherit (usname sval fi-alist class propagatep)
  (let* ((appendp 'no)
	 (related-frames
	  (and fi-alist
	       (some #'(lambda (fi-entry)
			 (dolist (sname (car fi-entry) nil)
			   (cond ((eq sname usname)
				  (setq appendp (cddr fi-entry))
				  (return (cadr fi-entry)))
				 ((and (consp sname)
				       (eq (car sname) usname))
				  (setq appendp
					(not (null (memq (cadr sname)
							 '(APPEND NCONC)))))
				  (return (cadr fi-entry))))))
		     fi-alist))))
    (cond (related-frames
	(cond ((eq appendp T)
	       (if (eq sval *DEFAULT*) (setq sval nil))
	       (setq sval (make-frame-slot class usname
					   (ia-facetplist-append
					    sval
					    (get-slot-ref usname related-frames
							  (arrayp related-frames) NIL))))
	       (if propagatep (setf (aref sval 1) 0)))
	      ((and (not (eq sval *DEFAULT*))
		    appendp)
	       (setq sval (make-frame-slot class usname sval)))
	      (T
	       (if (eq sval *DEFAULT*)
		   (setq sval
			 (or
			  (and related-frames
			       (make-slot-from-val class usname
						   (get-slot-ref usname related-frames
								 (arrayp related-frames)
								 :FIRST)))
			  sval))))))
	  ((not (eq sval *DEFAULT*))
	   (setq sval (make-frame-slot class usname sval))
	   (if propagatep (setf (aref sval 1) 0)))))
  sval)

;;; Re-written 22-Jan-87
(defun create-sf-entry (relname sval)
  (let ((urelname (keyword-to-user relname)))
    (list* (get-inherited-slot-names urelname)
	   sval
	   (get-appendp urelname))))

(defun ia-facetplist-append (pl1 l1)
  (cond (pl1
	 (nconc (cadr pl1) l1)
	 pl1)
	(T (list 'value l1))))

(defun make-slot-from-val (class usname val)
  (if val
      (make-frame-slot class usname (list 'value val 'depth 1))))

;;; Like make-frame except used by def-frame, which passes it a plist
;;; of non-keyword slots in a real list, and the frame type ftype.
;;; Makes a default frame (the same kind make-frame would return) but it
;;; assumes that all slots and facets are provided.
;;; The facet-plist for slots that have them are evaluated here.
;;; 7-Jan-87: Took eval-plist out since now eval is done at make time.
;;; 25-Jan-87: Put eval-plist back in here since it causes too many
;;; semantic and implementation hassles when you interpret class and instance
;;; slot values differently (e.g., you can inherit from and fire demons on
;;; both classes and instances).
(defun make-default-frame (class)
  (let* ((ftype (name-to-frame-type class))
	 (plist (pa-frame-slots ftype))
	 (newframe (make-array (pa-frame-numslots ftype)
			       :adjustable T))
	 (fi-plist (pa-frame-index-plist ftype))
	 sval snum usname)
    (dolist (sname (pa-frame-snames ftype))
      (setq usname (keyword-to-user sname))
      (setq snum (get-slot-num class fi-plist usname))
      (cond ((not (faceted-p2 fi-plist usname))
	     (setq sval (getf plist usname *DEFAULT*))
	     (if (eq sval *DEFAULT*)			     ;if not provided then
		 (error "Not all slot values were provided by def-frame.")))
	    (T (setq sval (getf plist usname *DEFAULT*))
	       (if (eq sval *DEFAULT*)
		   (error "Not all slot values were provided by def-frame.")
		   (setq sval (make-default-frame-slot (eval-plist sval))))))
      (setf (aref newframe snum) sval))
    (setf (aref newframe 0) class)
    (push newframe (get class 'instances))
    newframe))

;;; If the facet plist has no facets, then add the default one (value).
(defun parm-valueify (fplist)
  (or fplist
      '(value nil)))


;;;; Relation code

;;; Nondestructive. Returns the union of l1 and l2, guaranteeing that the
;;; order of the elts in the result is the same as the order in (append l1 l2).
(defun ordered-union (l1 l2)
  (do ((l2 l2 (cdr l2))
       (res nil (if (memq (car l2) l1) res (cons (car l2) res))))
      ((null l2) (append l1 (nreverse res)))))

(defun keyword-to-user (keyword)
  (intern (symbol-name keyword)))

(defun current-to-keyword (symbol)
  (intern (symbol-name symbol) (find-package "KEYWORD")))

(defun make-index-plist (plist1 plist2)
  (nconc (make-index-plist1 plist1) (make-index-plist1 plist2)))

(defun make-index-plist1 (plist)
  (do ((plist plist (cddr plist))
       (snum 0 (1+ snum))
       (new-plist nil
		  (cons (make-new-plist snum (cadr plist))
			(cons (car plist) new-plist))))
      ((null plist) (nreverse new-plist))))

(defun make-new-plist (snum facets)
  (cons snum (make-new-facets 0 facets)))

;;; EX: (1 (foo 'val1 bar 'val2)) ==> (foo 1 bar 2)
(defun make-new-facets (origin facets)
  (if (atom facets) facets
      (do ((fnum origin (1+ fnum))
	   (facets facets (cddr facets))
	   (res nil
		(cons fnum (cons (car facets) res))))
	  ((null facets) (nreverse res)))))

(defun combine-cache-slots (lcached all-slot-names full-cslots parents)
  (let ((all `(%class)))
    (do ((cslot full-cslots (cdr cslot))
	 (parent parents (cdr parent)))
	((null cslot)
	 (ordered-union all
			(translate-cache2 lcached all-slot-names)))
      (setq all (ordered-union all (translate-cache
				    (getf (car cslot) 'cache) (car parent)))))))

(defun translate-cache (val parent)
  (if (eq val '*ALL*)
      (mapcar #'keyword-to-user (cdr (get-slot-names parent)))	;;Avoid %class
      val))

(defun translate-cache2 (val snames)
  (if (eq val '*ALL*)
      snames
      val))

;;; Modified on 5-Dec-86 to only inherit slots specified by the 'cache
;;; class slot, instead of all of them as before.
(defun find-frame-slots (framenames)
  (mapcar
   #'(lambda (fname)
       (let ((cached-slots (get-cslot fname 'cache))
	     (all-specs (all-frame-slots fname)))
	 (if (eq cached-slots '*ALL*)
	     all-specs
	     (find-specs cached-slots all-specs fname))))
   framenames))

;;; Return the slot spec for every slot named in cached-slots.
(defun find-specs (cached-slots all-specs fname)
  (let (res)
    (dolist (cached-slot cached-slots (nreverse res))
      (let ((find (getf all-specs cached-slot)))
	(cond (find
	       (push cached-slot res)
	       (push find res))
	      (t (format t "?Cached slot ~A has no definition in class ~A~%"
			 cached-slot fname)))))))

(defun all-frame-slots (framename)
  (pa-frame-slots (name-to-frame-type framename)))

(defun find-frame-cslots (framenames)
  (mapcar
   #'(lambda (fname)
       (pa-frame-c-slots (name-to-frame-type fname)))
   framenames))

;;; Returns a plist associating the slots in the parents with the list
;;; of facets, making sure there is only one occurrence of each slot.
(defun combine-slots (frame-slots)
  (let (res)
    (dolist (frame-slot frame-slots (nreverse res))
      (do ((plist frame-slot (cddr plist)))
	  ((null plist))
	(when (not (memq (car plist) res))
	  (push (car plist) res)
	  (push (cadr plist) res))))))

;;; Since p2 implicitly has an is-a relation to p1, appendp is nil the first call
;;; to plist-union*.
(defun plist-union (p1 p2 propagatep relations)
  (do ((relations relations (cddr relations))
       (p2 (plist-union* p1 p2 propagatep NIL)
	   (plist-union* p2 (classify (find-inherited-slots relations))
			 propagatep (car relations))))
      ((null relations) p2)))

;;; Need to add a dummy class slot to the first slot plist so that the
;;; plist-union functions are guaranteed to have a class in the first position.
(defun classify (splist)
  (if (not (memq '%CLASS (car splist)))
      (cons (cons '%CLASS (cons nil (car splist)))
	    (cdr splist))
      splist))

(defun get-appendp (relation)
  (not (null (memq (get-slot relation 'combination-type) '(append nconc)))))

;;; Return the slot plist indicated by the relation (car relations) and related
;;; class (cadr relation).
(defun find-inherited-slots (relations)
  (get-slot-specs (car relations) (cadr relations)))

(defun get-slot-ref (slot-inherited relvalue facetp firstp)
  (if facetp
      (get-slot-faceted slot-inherited relvalue firstp)
      (get-slot-unfaceted slot-inherited relvalue)))

(defun get-slot-faceted (sname classslot firstp)
  (if firstp
      (get-first-slot-spec sname (aref classslot 0))
      (get-slot-spec0 sname (aref classslot 0))))

(defun get-first-slot-spec (sname framenames)
  (if framenames
      (get-value (car framenames) sname)))

;;; For unfaceted classslot
(defun get-slot-unfaceted (sname classslot)
  (get-value classslot sname))

(defun get-slot-spec0 (sname framenames)
  (mapcan #'(lambda (framename)
	      (copy-list (get-value framename sname)))
	  framenames))

;;; EX: (get-slot-specs 'part-of '(table1 table2))
;;; Return a list of the slot plists in the classes, using relation as the guide.
(defun get-slot-specs (relation classes)
  (let* ((slot-spec (get-inherited-slot-names relation)))
    (mapcar #'(lambda (class)
		(get-specified-slots
		 slot-spec
		 (pa-frame-slots (name-to-frame-type class))))
	    classes)))

;;; Like get-slot-specs except only returns a list of the slot names that
;;; are inherited through the relation; i.e., no plist since this is for
;;; the make-function writer, which doesn't know at writing time what the
;;; values are.
(defun get-inherited-slot-names (relation)
  (get-facet relation 'slots-inherited 'value))

;;; Return the slots specified in the list slot-spec, that are in the
;;; plist slots.
(defun get-specified-slots (slot-specs slots)
  (if (eq slot-specs '*ALL*) slots
      (let (sfiller sname res)
	(dolist (slot-spec slot-specs (nreverse res))
	  (setq sname (maybe-car slot-spec))
	  (when (setq sfiller (getf slots sname))
	    (push sname res) (push sfiller res))))))

(defun maybe-car (spec)
  (if (atom spec) spec (car spec)))

;;; Adds value facet iff valueifyp is T. Mangles p1.
(defun plist-union-no-propagate (p1 p2)
  (do ((plist2 p2 (cddr plist2))
       (res) (cached))
      ((null plist2) (nconc (nreverse res) p1))
    (let ((sname (car plist2)))
      (push sname res)
      (cond ((not (setq cached (memq sname p1)))
	     (push (cadr plist2) res))
	    (t (push (cadr cached) res)
	       (setf p1 (delete-slot sname p1)))))))

;;; Destructive version of combine-slots.  Added 18-Jan-87.
(defun combine-slots2 (frame-slots)
  (cond ((< (length frame-slots) 2)
	 (car frame-slots))
	(T
	 (let ((orig (nreverse (apply #'nconc frame-slots))))
	   (do ((plist orig (cdr prev))
		(prev (cdr orig) (cdr plist)))
	       ((null plist) (nreverse orig))
	     (cond ((eq plist orig)
		    (when (memq (car prev) (cddr prev))
		      (setf orig (cdr prev))))
		   (T
		    (if (memq (cadr plist) (cddr plist))
			(setf (cdr prev) (cdddr prev))))))))))

;;; Return the plist which is the plist-union of p1 & p2, p1 taking priority.
;;; Mangles p1 and p2.
;;; If propagatep is true then marker propagation is desired, so the depth and
;;; breadth of slots must be kept track of.
;;; If append is true then the common slots from p1 & p2 will be appended to make
;;; the final slot description; otherwise the slot descriptions in p1 will take
;;; precedence.
;;; With slot appending, propagating is not allowed [it's not clear that it
;;; would be useful, and it seems like a pain (if not impossible) to implement].
(defun plist-union* (p1 p2 propagatep relation)
  (let ((combotype (if (null relation) 'first
		       (get-slot relation 'combination-type)))
	(slotsin (if (null relation) '*ALL*
		     (get-value relation 'slots-inherited))))
    (plist-combine p1 (combine-slots2 p2) combotype slotsin propagatep)))

;;; Like plist-union-no-propagate except when both p1 and p2 contain
;;; a description of a slot, the result is to append the descriptions
;;; instead of using only p1's description.
;;; Re-written 18-Jan-87.  Only appends for the value facet, and since
;;; evaluation of class slots are done at make-time, doesn't actually
;;; append but sticks an '(append ...) around the things to append at
;;; make time.
;;; This is a merging of plist-append and plist-union-no-propagate
;;; (19-Jan-87).
;;; Modified 25-Jan-87 to also do propagate bookkeeping iff propagatep.
(defun plist-combine (p1 p2 combotype slotsin propagatep)
  (do ((plist2 p2 (cddr plist2))
       (res) (cached))
      ((null plist2) (nconc (nreverse res) (propagate-init-plist propagatep p1)))
    (let ((sname (car plist2)))
      (push sname res)
      (cond ((not (setq cached (memq sname p1)))
	     (push (propagate-update propagatep (cadr plist2)) res))
	    (T (push (propagate-init propagatep
		      (maybe-fplist-append (cadr cached) (cadr plist2)
					   sname combotype slotsin)) res)
	       (setf p1 (delete-slot sname p1)))))))

;;; Added 19-Jan-87.  Like facetplist-append if slotsin specifies append
;;; for the given slot sname, or if slotsin doesn't specify anything for
;;; sname and combotype is append; else uses first.
;;; Modified 25-Jan-87 to initialize the depth facet if necessary.
(defun maybe-fplist-append (fp1 fp2 sname default-combo slotsin)
  (let ((combo (get-combotype default-combo sname slotsin)))
    (cond ((and (or (eq combo 'append) (eq combo 'nconc))
		(consp fp1) (consp fp2))
	   `(value (,combo ,(cadr fp1) ,(cadr fp2))
		   ,@(cddr fp1)))
	  (T fp1))))

(defun get-combotype (default sname slotsin)
  (or (and (not (eq slotsin '*ALL*))
	   (cadar (member-if #'(lambda (sentry)
				 (and (consp sentry)
				      (eq (car sentry) sname)
				      (cadr sentry)))
			     slotsin)))
      default))

;;; Modified 25-Jan-87 to work with plist-combine.
(defun propagate-init (propagatep facet-plist)
  (if (and propagatep (consp facet-plist) (not (memq 'depth facet-plist)))
      (setf (cddr facet-plist) (append '(depth 0) (cddr facet-plist))))
  facet-plist)

;;; Modified 25-Jan-87 to work with plist-combine.
(defun propagate-init-plist (propagatep plist)
  (let ((orig plist))
    (do ((plist (cdr plist) (cddr plist)))
	((null plist) orig)
      (if (and (consp (car plist)) (not (memq 'value (car plist))))
	  (setf (car plist) (append '(value nil) (car plist))))
      (and (car plist) propagatep
	   (setf (car plist) (propagate-init T (car plist))))))
  plist)

;;; Modified 25-Jan-87 to work with plist-combine.
(defun propagate-update (propagatep facet-plist)
  (cond ((not propagatep)
	 (parm-valueify facet-plist))
	(T (if (consp facet-plist)
	       (incf (getf facet-plist 'depth)))
	   facet-plist)))

;;; Destructively deletes the slot with the given name from the plist
;;; and returns the new plist.
(defun delete-slot (sname plist)
  (let ((orig plist))
    (do ((plist plist (cddr plist))
	 (backone nil plist))
	((eq sname (car plist))
	 (if backone
	     (setf (cddr backone) (cddr plist))
	     (setf orig (cddr orig)))
	 orig)
      (if (null plist) (return orig)))))


;;; Slot accessor functions, book-keeping.

;;; Define the frame-slot accessors and storers (setf methods) since
;;; the frame is adjustable and aref is needed.
;;; Also writes the maker function, with same syntax as defstruct.
;;; Added 27-Sep-86:  setfable parameter.  Only write the setf methods
;;; if setfable is T.
(defun write-accessor-functions
       (theframe has-facets name plist setfable propagatep)
  (do* ((slot-names plist (cddr slot-names))
	(sname (car slot-names) (car slot-names))
	(frame-slot (smash name "-" sname) (smash name "-" sname))
	(slotnum 0 (1+ slotnum)))
       ((null slot-names)
	(setf (pa-frame-numslots theframe) slotnum))
    (define-slot-accessor frame-slot slotnum)
    (when (and has-facets (not (atom (cadr slot-names))))
      (write-facet-refs frame-slot slotnum (cadr slot-names)
			setfable propagatep))
    (when setfable
      (define-slot-setf-method frame-slot slotnum :ext))))

(defun dynamic-define-generic-slot-accessor (snum fnum access)
  `(,access (,access frame ,snum) ,fnum))

;;; Dynamic programming technique. Given snum, fnum, check to see if the
;;; access function for slot snum, facet fnum has yet been defined.  If
;;; not, then define it and store it so the next time it won't have to be
;;; redefined.
;;; plist is passed by name so this function can push onto it.
(defun get-generic-slot-accessor (fn-name snum fnum plist access)
  (let ((fnum-plist (memq snum (symbol-value plist))))
    (cond (fnum-plist
	   (let ((slot-facet-fn (getf (cadr fnum-plist) fnum)))
	     (cond (slot-facet-fn
		    (pa-eval
		     `(defun ,fn-name (frame)
			,slot-facet-fn)))
		   (t
		    (let ((new-fn
			  (dynamic-define-generic-slot-accessor snum fnum access)))
		      (push new-fn (cadr fnum-plist))
		      (push fnum (cadr fnum-plist))
		      (pa-eval
		       `(defun ,fn-name (frame)
			  ,new-fn)))))))
	 (t (let ((new-fn (dynamic-define-generic-slot-accessor snum fnum access)))
	      (push (list fnum new-fn) (symbol-value plist))
	      (push snum (symbol-value plist))
	      (pa-eval `(defun ,fn-name (frame)
			  ,new-fn)))))))

(defun get-aref-slot-accessor (fn-name snum fnum)
  (get-generic-slot-accessor fn-name snum fnum '*AREF-SLOT-FN-PLIST* 'aref))

(defun define-slot-accessor (frame-slot slotnum)
  (define-aref-slot-accessor frame-slot slotnum))

(defun define-aref-slot-accessor (frame-slot slotnum)
  (if (<= slotnum 15)
      (pa-eval `(setf (symbol-function ',frame-slot)
		      (symbol-function ',(aref *AREF-FN-MAP* slotnum))))
      (pa-eval
       `(defun ,frame-slot (frame)
	  (aref frame ,slotnum)))))

(defun define-ref-slot-facet-accessor (fname snum fnum)
  (define-aref-slot-facet-accessor fname snum fnum))

(defun define-aref-slot-facet-accessor (fn-name snum fnum)
  (cond ((and (= fnum 0) (<= snum 15))
	 (pa-eval `(setf (symbol-function ',fn-name)
			 (symbol-function ',(aref *AREF-FACET1-FN-MAP* snum)))))
	(t
	 (get-aref-slot-accessor fn-name snum fnum))))

;;; Example of what this function would write given house-height and 1:
;;; (defsetf house-height (frame) (newval)
;;;       `(setf (aref ,frame 1) ,newval))
(defun define-slot-setf-method (frame-slot slotnum ext)
  (let ((ref (if ext 'aref 'svref)))
    (pa-eval
     `(defsetf ,frame-slot (frame) (newval)
        `(setf (,',ref ,frame ,',slotnum) ,newval)))))

;;; Add: maybe write frame-slot-facet accessor given a slot.
;;; Sample facet accessor: given house, size, 1, (value depth):
;;; (defun house-size.value (frame) (aref (aref frame 1) 0))
;;; (defun house-size.depth (frame) (aref (aref frame 1) 1))
;;; (defsetf house-height.value (frame) (newval)
;;;       `(setf (aref (aref ,frame 1) 0) ,newval))
;;; (defsetf house-height.depth (frame) (newval)
;;;       `(setf (aref (aref ,frame 1) 1) ,newval))
;;; As of 7-Sep-86, it only defines functions and setf methods for the 0th facet,
;;; because it's very expensive to do so for all facets.
(defun write-facet-refs (frame-slot snum facet-names setfable propagatep)
  (let* ((facet-prefix (smash frame-slot "."))
	 (slot-prefix (smash frame-slot "-"))
	 (ref (if :ext 'aref 'svref)))
    (let* ((facet-name (car facet-names))
	   (fn-name (smash facet-prefix facet-name)))
      (define-slot-accessor (smash slot-prefix facet-name) 0)
      (define-ref-slot-facet-accessor fn-name snum 0)
      (if setfable (define-facet-setf-method fn-name ref snum 0 propagatep)))))

(defun define-facet-setf-method (fn-name ref snum fnum propagatep)
  (pa-eval
   (if (and propagatep (= fnum 0))	;;0 is the value facet (usually)
    `(defsetf ,fn-name (frame) (newval) 
      `(progn
	(setf (,',ref (,',ref ,frame ,',snum) 1) 0)	;;setting depth to 0 means
	(setf (,',ref (,',ref ,frame ,',snum) ,',fnum) ,newval)))  ;;it's cached.
     `(defsetf ,fn-name (frame) (newval)
        `(setf (,',ref (,',ref ,frame ,',snum) ,',fnum) ,newval)))))


(defun name-to-frame (frame-name)
  (get frame-name :frame))

;;; Same thing, just has a better name and is documented.
(defun frame (frame-name)
  (get frame-name :frame))

(defun isa-p (name1 name2)
  (or (eq name1 name2)
      (memq name2 (isas name1))))

;;; Returns T iff the given name names a frame class.
(defun frame-class-p (frame-name)
  (get frame-name 'classp))

(defun name-to-frame-type (frame-name)
  (get frame-name :frame-type))

(defun get-slot-names (framename)
  (pa-frame-snames (name-to-frame-type framename)))


(eval-when (eval load compile)
  (def-frame relation ()
    combination-type first
    slots-inherited (value '*ALL*))
  ;;  cache-slots NIL	    ;;If T, then propagate and cache inherited slots.

  )
;;; The IS-A and PART-OF relations
;;; (def-frame is-a (is-a (relation) propagate nil)
;;;  combination-type first
;;;  slots-inherited (value '*ALL*))

(def-frame part-of (is-a (relation) propagate nil)
  combination-type first
  slots-inherited (value '((location first) (made-from append))))
