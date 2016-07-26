;;
;;Received: from K.GP.CS.CMU.EDU by ML.RI.CMU.EDU; 13 Jul 88 16:27:08 EDT
;;Received: from K.GP.CS.CMU.EDU by K.GP.CS.CMU.EDU; 13 Jul 88 16:23:16 EDT
;;To: pshell@ml
;;Subject: Bugs?
;;Date: Wed, 13 Jul 88 16:23:02 EDT
;;Message-ID: <28149.584828582@K.GP.CS.CMU.EDU>
;;From: Paul.Birkel@K.GP.CS.CMU.EDU
;;Status: R


;;; -*- Mode: Lisp; Package: USER -*-

;(load "dm:parmenides")
;(load "dm:build")
;(load "dm:inter")

(def-frame related-to (:is-a relation :propagate nil)
  :combination-type :FIRST
  :slots-inherited  (:value '*ALL*)
  :has-inverses nil)

(def-frame event (:cache *ALL* :propagate nil)
  name nil
  patient-id nil
  when (:value 0 :date nil :time 0)
  meal (:value nil)
  )

(def-frame treatable-event (:is-a (event) :cache *ALL* :propagate nil)
  related-to nil)

(def-frame meal-event (:is-a (treatable-event) :cache *ALL*))

(def-frame non-meal-event (:is-a (treatable-event) :cache *ALL*))

(def-frame reaction-event (:is-a (treatable-event)
			    :related-to (non-meal-event)
		            :cache *ALL*))


(make-frame 'meal-event 'event-1 :name 'event-1 :patient-id 6
   :when '(:value 2750086800 :date Feb-23-87 :time 1200)
   :meal '(:value lunch))

(make-frame 'non-meal-event 'event-2 :name 'event-2 :patient-id 6
   :when '(:value 2750198400 :date Feb-24-87 :time 1900)
   :meal '(:value evening))

;; This one will make a wme but the contents are screwed-up/missing.
;;(make-frame 'reaction-event 'event-3 :related-to 'event-2)


(def-frame stuff (related-to non-meal-event) :related-to nil :stuff-slot nil)

;; This one will break completely
;;(make-stuff 'stuff1 :related-to 'event-2)


;;-- paul

