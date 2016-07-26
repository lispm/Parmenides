#|
Defining class PARENT
Defining class COMPONENTS
Defining class NODE
Defining class HIERARCHY-OBJECT
Defining class HOTEL
Defining class ATRIUM
[#<Vector T 13 A37743> is not a frame, though #<Vector T 13 A27243> has relation :COMPON
ENTS to it.]
[NIL is not a frame, though #<Vector T 13 A27243> has relation :COMPONENTS to it.]
#P"/usr.MC68020/iesl/groleau/groleau/destiny/test"
|#
;*******************
;     LINKS
;*******************

(defvar *MY-DEFAULT* '(NIL))

(def-frame parent (is-a relation)
  has-inverses t
  inverse-name components)

;*******************
;     CLASSES
;*******************

(setq node ;THE GRAPHIC CLASS
      (frame
       (def-frame node (cache *ALL*) ;ALL SLOTS MUST BE PROPAGATED
  linked-to-root (value *MY-DEFAULT*)
  components (value *MY-DEFAULT*)
  parent (value *MY-DEFAULT*)
  x (value *MY-DEFAULT* changeable t)
  y (value *MY-DEFAULT* changeable t)
  space (value *MY-DEFAULT* changeable t))))

(setq hierarchy-object
      (frame
       (def-frame hierarchy-object (is-a node cache *ALL*)
  posted (value *MY-DEFAULT*)
  modified (value *MY-DEFAULT*)
  deleted (value *MY-DEFAULT*)
  history (value *MY-DEFAULT*)
  current-solution (value *MY-DEFAULT*
			  post-if-set '(notify-kms)))))

;TOP LEVEL
(setq hotel
      (frame
       (def-frame hotel (is-a (hierarchy-object))
  name (value "hotel" changeable t))))

;GENERAL LAYOUT
(setq atrium
      (frame
       (def-frame atrium (is-a (hierarchy-object))
  name (value "atrium" changeable t)
  parent (value 'hotel))))
