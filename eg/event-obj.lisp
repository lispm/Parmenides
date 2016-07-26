;;; -*- Mode: Lisp; Package: USER -*-

(format t "~& Loading event objects.~%")

;; This relation is used in moving instances between classes.

(def-frame related-to (:is-a relation :propagate nil)
  :combination-type :FIRST
  :slots-inherited  (:value '*ALL*)
  :has-inverses nil)

;; NOTE: The initial database is assumed to correctly assign instances
;;   to either MEAL, NON-MEAL, or COMMENT events. Subsequent inferencing
;;   may  move NON-MEAL events to the REACTION-WO-MEAL subclass.

;;    event
;;        treatable-event
;;            meal-event                    <--+- hang instances at these nodes
;;            non-meal-event                <--|
;;                reaction-wo-meal-event    <--|
;;        comment-event                     <--'


(literalize event (:cache *ALL*)
  name nil
  patient-id nil
  when (:value 0 :date nil :time 0)
  meal (:value nil)
  bg (:value nil  :hyperglycemia nil  :hypoglycemia nil
      :status-wrt-predicted nil
      :predicted-symbolic-value nil  :predicted-range nil)
  urine (:sugar nil                 :acetone nil
	 :predicted-sugar-range nil :predicted-acetone-range nil
	 :sugar-wrt-predicted nil   :acetone-wrt-predicted nil)
  longact (:value 0)
  dose (:value 0)
  mod (:value 0  :original nil  :diff-explained-by nil)
  comment (:value nil  :duration nil  :state nil  :parsed nil
	   :explain-missing-bg nil  :explain-extra-bg nil
	   :reaction-degree nil  :treated-reaction 'UNKNOWN
	   :explain-dose-change nil  :dose-type 'REGULAR  :dose-adjustment 0
	   :explain-calorie-change nil  :meal-timing nil
	   :activity-type nil  :activity-degree nil
	   :calorie-type nil  :calorie-degree nil
	   :physiology-type nil  :physiology-degree nil
	   :emotion-type nil  :emotion-degree nil)
  reg-mod (:value nil         :evaluated nil
	   :summary-list nil  :plausible-list nil
	   :rebound nil
	   :increased-activity nil  :decreased-activity nil
	   :increased-calorie nil   :decreased-calorie nil
	   :immediate-ketonuria nil :preceding-ketonuria nil
	   :illness nil     :menses nil
	   :anger nil       :joy nil
	   :excitement nil  :grief nil
	   :anxiety nil     :asleep nil
	   :admin-error nil :switch-meal nil :unknown nil)
  CSI-mod  ;; value is a list of the amount and the duration
          (:value nil         :evaluated nil
	   :summary-list nil  :plausible-list nil
	   :rebound nil
	   :increased-activity nil  :decreased-activity nil
	   :increased-calorie nil   :decreased-calorie nil
	   :immediate-ketonuria nil :preceding-ketonuria nil
	   :illness nil     :menses nil
	   :anger nil       :joy nil
	   :excitement nil  :grief nil
	   :anxiety nil     :asleep nil
	   :admin-error nil :switch-meal nil :unknown nil)
  LA-mod  (:value nil         :evaluated nil
	   :summary-list nil  :plausible-list nil
	   :increased-activity nil  :decreased-activity nil
	   :misc nil        :unknown nil)
  calorie-mod (:value nil         :evaluated nil
	       :summary-list nil  :plausible-list nil
	       :reaction nil      :increased-activity nil)
  ;
  compliance-problems (:value nil)
  data-collection-problems (:missing-bg nil  :missing-S&A nil)
  ;
  explained-by (:value nil  :zone nil  :day nil  :week nil))


(literalize treatable-event (:is-a (event) :cache *ALL*)
  related-to nil                  ;; used for moving instances
  prev-meal-event (:value nil)
  next-meal-event (:value nil)
  reg-mod (:value 0)
  CSI-mod (:value 0)
  LA-mod (:value 0))


(literalize meal-event (:is-a (treatable-event) :cache *ALL*)
  related-event-treats(:increased-activity nil  :decreased-activity nil
		       :increased-calorie nil)
  timing-problems (:inter-zone-interval nil  :inter-meal-interval nil)
  simulate (:intra-zone-meals nil  :in-progress nil)
  ;
  pre-meal-events (:value nil)
  post-meal-events (:value nil)
  ;
  prev-day-same-meal (:value nil)
  next-day-same-meal (:value nil)
  prev-week-same-meal (:value nil)
  next-week-same-meal (:value nil))


(literalize non-meal-event (:is-a (treatable-event) :cache *ALL*))

(literalize reaction-wo-meal-event (:is-a (non-meal-event)
		                    :related-to (non-meal-event)
				    :cache *ALL*))


(literalize comment-event (:is-a event :cache *ALL*))

