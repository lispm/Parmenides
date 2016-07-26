;;; -*- Mode: Lisp; Package: USER -*-

(def-frame root (:cache *ALL*))

(def-frame plan-generated (:is-a (root) :cache *ALL*)
  stepslots-list (:value nil)
  goal-unit (:value nil))

(def-frame problem-statement (:is-a (root) :cache *ALL*)
  goal (:value nil)
  constraints (:value nil))




