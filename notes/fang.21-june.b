Received: from CIVE.RI.CMU.EDU by ML.RI.CMU.EDU; 21 Jun 87 18:01:12 EDT
Date: Sunday, 21 June 1987 17:59:52 EDT
From: Fang.Zhao@cive.ri.cmu.edu
To: pshell@cive.ri.cmu.edu
Subject: How to make a frame?
Message-ID: <1987.6.21.21.33.4.Fang.Zhao@cive.ri.cmu.edu>

Peter,  I have changed my 'is-part-of' to a slot with facet and tried my
test file.  It loaded.  But when I tried to make an instance of door, it
blew me up again.  I don't know if I used the wrong format.  Can you tell me
how?  Thanks.  --Fang

> (load "test")
Defining class IS-PART-OF
Defining class HAS
Defining class TRUCK
Defining class DOOR
Defining class HANDLE
#P"/usrce0/fzhao/hi-rise/test.lisp"
> (pp-frame 'door)
Frame Class DOOR: (:CACHE (:%CLASS) IS-PART-OF (TRUCK) :HAS (HANDLE))
MAT             STEEL
WEIGHT          1000
IS-PART-OF      (:VALUE TRUCK :DEPTH 0)
COLOR           RED
HAS             (:VALUE (HANDLE) :DEPTH 0)
NIL
> (pp-frame 'handle)
Frame Class HANDLE: (:CACHE (:%CLASS) IS-PART-OF (DOOR))
MAT             STEEL
WEIGHT          1000
IS-PART-OF      (:VALUE DOOR :DEPTH 0)
COLOR           RED
LENGTH          20
NIL
> (make-truck 'truck-1 :weight 500 :mat 'p)
#<Vector T 4 569C13>
> (pp-frame 'truck-1)
%CLASS          TRUCK
MAT             P
WEIGHT          500
HAS             (:VALUE (DOOR) :DEPTH 1)
NIL
> (make-door 'door-1 :is-part-of '(value truck-1) :color 'green)
>>Error: In function AREF, the index arg 2 for the 
     vector #<Simple-Vector T 2 6017FB> is not a fixnum in range  [0, 2).

AREF:
   Required arg 0 (ARRAY): #<Simple-Vector T 2 6017FB>
   Optional arg 1 (I1): 2
   Optional arg 2 (I2): #<Unbound 121E>
   Rest arg (MORE-SUBSCRIPTS): NIL

:A    Abort to Lisp Top Level
:C    Supply an index.
-> :a
Back to Lisp Top Level

> (make-door 'door-1 :is-part-of '(value 'truck-1) :color 'green)
>>Error: The argument to AREF, NIL, is not an array.

AREF:
   Required arg 0 (ARRAY): NIL
   Optional arg 1 (I1): 0
   Optional arg 2 (I2): #<Unbound 121E>
   Rest arg (MORE-SUBSCRIPTS): NIL

:A    Abort to Lisp Top Level
:C    Supply an array.
-> :a
Back to Lisp Top Level


>

