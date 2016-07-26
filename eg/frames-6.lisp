;; -*- Mode: Lisp; Package: USER -*-

(format t "~& Loading patient-6 frames.~%")
#|
($make-named 'patient 'patient-6 :name 'patient-6 :patient-id 6
   :patient-name '(:last Reed :first Valerie)
   :demographics '(:birthdate Oct-17-62 :sex F :race W)
   :date-in-first-week '(:value Feb-23-87)
   :date-in-last-week  '(:value Mar-20-87)
   :loaded '(:value t))

(make-network-state 'network-state-6 :name 'network-state-6 :patient-id 6
   :first-monday-date '(:value Feb-23-87)
   :last-monday-date '(:value Mar-16-87)
   :network-state '(:monday-date Feb-23-87
		    :first-meal event-545      :last-meal event-674
		    :first-breakfast event-545 :last-breakfast event-674
		    :first-lunch event-546     :last-lunch event-670
		    :first-dinner event-547    :last-dinner event-671
		    :first-snack event-548     :last-snack event-672))

(make-patient-state 'patient-state-6 :name 'patient-state-6 :patient-id 6
   :activity-state
   '(:value (#S(activity-change :when 2750043600)
	       #S(activity-change :when 2751807600 :activity-type none)
	       #S(activity-change :when 2751820200)))
   :calorie-state
   '(:value (#S(calorie-change :when 2750043600)))
   :emotion-state
   '(:value (#S(emotion-change when 2750043600)))
   :physiology-state
   '(:value (#S(physiology-change when 2750043600)
	       #S(physiology-change when 2752030800 changed-components (illness)
				    illness moderate)
	       #S(physiology-change when 2752146000 changed-components (illness)
				    illness none))))

(make-management-model 'management-model-6 :patient-id 6
   :when '(:value 2750043600 :date 'Feb-23-87)
   :meal-times '(:breakfast 800 :lunch 1200 :dinner 1800 :snack 2230)
   :next-change 'management-model-6-feb-24-87
   :high-goal '(:value (130 149 169 208 286))
   :goal-ranges
   '(:mod  (   -2      -1        0        +1        +2        +3)
     :band ((0 52) (48 74) (66 136) (124 210) (190 262) (238 999)))
   :zone-model '(:value intensive-zones)
   :breakfast-reg '(:value 5 :scale-mod (-2 -1  0   1   2   3)
			     :scale-band (0 50 69 130 200 250))
   :lunch-reg     '(:value 4 :scale-mod (-2 -1  0   1   2   3)
		             :scale-band (0 50 69 130 200 250))
   :dinner-reg    '(:value 6 :scale-mod (-2 -1  0   1   2   3)
		             :scale-band (0 50 69 130 200 250))
   :snack-reg     '(:value 2 :scale-mod (-2 -1  0   1   2)
			     :scale-band (0 68 80 150 250)
			     :predicate 'always)
   :long-act '(:value CSI)
   :breakfast-LA '(:value 1.1)
   :activity '(:increased-reg-mod (-1 -1 -2)
	       :decreased-reg-mod ( 0  1  2))
   :calorie  '(:increased-reg-mod ( 0  1  2)
	       :decreased-reg-mod ( 0 -1 -2)))

(make-management-model-change 'management-model-6-feb-24-87 :patient-id 6
   :when '(:value 2750130000 :date 'Feb-24-87)
   :prev-change 'management-model-6
   :next-change 'management-model-6-mar-06-87
   :breakfast-reg '(:value 6))

(make-management-model-change 'management-model-6-mar-06-87 :patient-id 6
   :when '(:value 2750994000 :date 'Mar-06-87)
   :prev-change 'management-model-6-feb-24-87
   :next-change 'management-model-6-mar-07-87
   :lunch-reg '(:value 3))

(make-management-model-change 'management-model-6-mar-07-87 :patient-id 6
   :when '(:value 2751080400 :date 'Mar-07-87)
   :prev-change 'management-model-6-mar-06-87
   :next-change 'management-model-6-mar-11-87
   :breakfast-reg '(:value 5))

(make-management-model-change 'management-model-6-mar-11-87 :patient-id 6
   :when '(:value 2751426000 :date 'Mar-11-87)
   :prev-change 'management-model-6-mar-07-87
   :next-change 'management-model-6-mar-13-87
   :activity '(:increased-CSI-mod ((0 0) (-1 60) (-1 60))))

(make-management-model-change 'management-model-6-mar-13-87 :patient-id 6
   :when '(:value 2751598800 :date 'Mar-13-87)
   :prev-change 'management-model-6-mar-11-87
   :next-change 'management-model-6-mar-19-87
   :breakfast-LA '(:value 1.2))

(make-management-model-change 'management-model-6-mar-19-87 :patient-id 6
   :when '(:value 2752118200 :date 'Mar-19-87)
   :prev-change 'management-model-6-mar-13-87
   :breakfast-reg '(:value 6))
|#
($make-named 'meal-event 'event-545 :name 'event-545 :patient-id 6
   :when '(:value 2750072400 :date Feb-23-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 112)
   :urine '(:sugar 2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 5) :mod '(:value 0)
   :comment '(:value (no milk #\; sinus headache #\; busy around house)
              :activity-type increased
              :activity-degree mild
              :calorie-type decreased
              :calorie-degree mild
              :physiology-type illness
              :physiology-degree mild)
   :prev-meal-event '(:value nil) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-546)
   :prev-day-same-meal '(:value nil) :next-day-same-meal '(:value event-549)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-576))

($make-named 'meal-event 'event-546 :name 'event-546 :patient-id 6
   :when '(:value 2750086800 :date Feb-23-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 65)
   :dose '(:value 4.1) :mod '(:value 0.1)
   :comment '(:value (did not clamp tubing #\, 1.1 extra)
              :parsed t
              :explain-dose-change admin-error
              :dose-type reg
              :dose-adjustment 1.1)
   :prev-meal-event '(:value event-545) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-547)
   :prev-day-same-meal '(:value nil) :next-day-same-meal '(:value event-550)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-577))

($make-named 'meal-event 'event-547 :name 'event-547 :patient-id 6
   :when '(:value 2750108400 :date Feb-23-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 155)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (-1 for housework)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -1
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-546) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-548)
   :prev-day-same-meal '(:value nil) :next-day-same-meal '(:value event-551)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-578))

($make-named 'meal-event 'event-548 :name 'event-548 :patient-id 6
   :when '(:value 2750124600 :date Feb-23-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 127)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-547) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-549)
   :prev-day-same-meal '(:value nil) :next-day-same-meal '(:value event-553)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-579))

($make-named 'meal-event 'event-549 :name 'event-549 :patient-id 6
   :when '(:value 2750158800 :date Feb-24-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 75)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 5) :mod '(:value -1)
   :comment '(:value (-1 for driving)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -1
              :activity-type increased
              :activity-degree mild)
   :prev-meal-event '(:value event-548) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-550)
   :prev-day-same-meal '(:value event-545) :next-day-same-meal '(:value event-555)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-580))

($make-named 'meal-event 'event-550 :name 'event-550 :patient-id 6
   :when '(:value 2750173200 :date Feb-24-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 187)
   :dose '(:value 5) :mod '(:value 1)
   :prev-meal-event '(:value event-549) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-551)
   :prev-day-same-meal '(:value event-546) :next-day-same-meal '(:value event-556)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-581))

($make-named 'meal-event 'event-551 :name 'event-551 :patient-id 6
   :when '(:value 2750194800 :date Feb-24-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 78)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-550) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-552)) :next-meal-event '(:value event-553)
   :prev-day-same-meal '(:value event-547) :next-day-same-meal '(:value event-557)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-583))

($make-named 'non-meal-event 'event-552 :name 'event-552 :patient-id 6
   :when '(:value 2750198400 :date Feb-24-87 :time 1900)
   :meal '(:value evening)
   :comment '(:value (watching exciting basketball game #\; junk food)
              :parsed t
              :calorie-type increased
              :calorie-degree severe
              :emotion-type excitement
              :emotion-degree moderate)
   :prev-meal-event '(:value event-551) :next-meal-event '(:value event-553))

($make-named 'meal-event 'event-553 :name 'event-553 :patient-id 6
   :when '(:value 2750211000 :date Feb-24-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 270)
   :dose '(:value 4) :mod '(:value 2)
   :prev-meal-event '(:value event-551) :pre-meal-events '(:value (event-552))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-555)
   :prev-day-same-meal '(:value event-548) :next-day-same-meal '(:value event-558)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-584))

($make-named 'comment-event 'event-554 :name 'event-554 :patient-id 6
   :when '(:value 2750130000 :date Feb-24-87 :time 0)
   :comment '(:value (x increased to 6)
              :parsed t))

($make-named 'meal-event 'event-555 :name 'event-555 :patient-id 6
   :when '(:value 2750245200 :date Feb-25-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 147)
   :urine '(:sugar 5% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 7) :mod '(:value 1)
   :comment '(:value (out driving)
              :parsed t
              :activity-type increased
              :activity-degree mild)
   :prev-meal-event '(:value event-553) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-556)
   :prev-day-same-meal '(:value event-549) :next-day-same-meal '(:value event-559)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-585))

($make-named 'meal-event 'event-556 :name 'event-556 :patient-id 6
   :when '(:value 2750259600 :date Feb-25-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 162)
   :dose '(:value 5) :mod '(:value 1)
   :prev-meal-event '(:value event-555) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-557)
   :prev-day-same-meal '(:value event-550) :next-day-same-meal '(:value event-560)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-586))

($make-named 'meal-event 'event-557 :name 'event-557 :patient-id 6
   :when '(:value 2750281200 :date Feb-25-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 120)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-556) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-558)
   :prev-day-same-meal '(:value event-551) :next-day-same-meal '(:value event-561)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-588))

($make-named 'meal-event 'event-558 :name 'event-558 :patient-id 6
   :when '(:value 2750297400 :date Feb-25-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 117)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-557) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-559)
   :prev-day-same-meal '(:value event-553) :next-day-same-meal '(:value event-562)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-589))

($make-named 'meal-event 'event-559 :name 'event-559 :patient-id 6
   :when '(:value 2750331600 :date Feb-26-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 99)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (slept on couch)
              :parsed t
              :activity-type decreased
              :activity-degree moderate)
   :prev-meal-event '(:value event-558) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-560)
   :prev-day-same-meal '(:value event-555) :next-day-same-meal '(:value event-563)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-590))

($make-named 'meal-event 'event-560 :name 'event-560 :patient-id 6
   :when '(:value 2750346000 :date Feb-26-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 197)
   :dose '(:value 5) :mod '(:value 1)
   :prev-meal-event '(:value event-559) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-561)
   :prev-day-same-meal '(:value event-556) :next-day-same-meal '(:value event-564)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-591))

($make-named 'meal-event 'event-561 :name 'event-561 :patient-id 6
   :when '(:value 2750367600 :date Feb-26-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 103)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-560) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-562)
   :prev-day-same-meal '(:value event-557) :next-day-same-meal '(:value event-565)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-593))

($make-named 'meal-event 'event-562 :name 'event-562 :patient-id 6
   :when '(:value 2750383800 :date Feb-26-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 131)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-561) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-563)
   :prev-day-same-meal '(:value event-558) :next-day-same-meal '(:value event-566)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-594))

($make-named 'meal-event 'event-563 :name 'event-563 :patient-id 6
   :when '(:value 2750418000 :date Feb-27-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 113)
   :urine '(:sugar neg :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-562) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-564)
   :prev-day-same-meal '(:value event-559) :next-day-same-meal '(:value event-568)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-596))

($make-named 'meal-event 'event-564 :name 'event-564 :patient-id 6
   :when '(:value 2750432400 :date Feb-27-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 69)
   :dose '(:value 3) :mod '(:value -1)
   :prev-meal-event '(:value event-563) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-565)
   :prev-day-same-meal '(:value event-560) :next-day-same-meal '(:value event-569)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-597))

($make-named 'meal-event 'event-565 :name 'event-565 :patient-id 6
   :when '(:value 2750454000 :date Feb-27-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 189)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (presumed rebound #\; ponderosa #\, ate extra)
              :parsed t
              :explain-dose-change rebound
              :dose-type reg
              :dose-adjustment -1
              :calorie-type increased
              :calorie-degree moderate)
   :prev-meal-event '(:value event-564) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-566)
   :prev-day-same-meal '(:value event-561) :next-day-same-meal '(:value event-570)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-599))

($make-named 'meal-event 'event-566 :name 'event-566 :patient-id 6
   :when '(:value 2750470200 :date Feb-27-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 163)
   :dose '(:value 3) :mod '(:value 1)
   :prev-meal-event '(:value event-565) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-567)) :next-meal-event '(:value event-568)
   :prev-day-same-meal '(:value event-562) :next-day-same-meal '(:value event-571)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-600))

($make-named 'non-meal-event 'event-567 :name 'event-567 :patient-id 6
   :when '(:value 2750491800 :date Feb-27-87 :time 2830)
   :meal '(:value post-snack)
   :bg '(:value 227)
   :prev-meal-event '(:value event-566) :next-meal-event '(:value event-568))

($make-named 'meal-event 'event-568 :name 'event-568 :patient-id 6
   :when '(:value 2750504400 :date Feb-28-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 197)
   :urine '(:sugar 2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 7) :mod '(:value 1)
   :prev-meal-event '(:value event-566) :pre-meal-events '(:value (event-567))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-569)
   :prev-day-same-meal '(:value event-563) :next-day-same-meal '(:value event-572)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-602))

($make-named 'meal-event 'event-569 :name 'event-569 :patient-id 6
   :when '(:value 2750518800 :date Feb-28-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 168)
   :dose '(:value 5) :mod '(:value 1)
   :comment '(:value (ate extra)
              :parsed t
              :calorie-type increased
              :calorie-degree moderate)
   :prev-meal-event '(:value event-568) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-570)
   :prev-day-same-meal '(:value event-564) :next-day-same-meal '(:value event-573)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-603))

($make-named 'meal-event 'event-570 :name 'event-570 :patient-id 6
   :when '(:value 2750540400 :date Feb-28-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 230)
   :dose '(:value 8) :mod '(:value 2)
   :prev-meal-event '(:value event-569) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-571)
   :prev-day-same-meal '(:value event-565) :next-day-same-meal '(:value event-574)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-604))

($make-named 'meal-event 'event-571 :name 'event-571 :patient-id 6
   :when '(:value 2750556600 :date Feb-28-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 152)
   :dose '(:value 3) :mod '(:value 1)
   :prev-meal-event '(:value event-570) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-572)
   :prev-day-same-meal '(:value event-566) :next-day-same-meal '(:value event-575)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-605))

($make-named 'meal-event 'event-572 :name 'event-572 :patient-id 6
   :when '(:value 2750590800 :date Mar-01-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 104)
   :urine '(:sugar 1% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-571) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-573)
   :prev-day-same-meal '(:value event-568) :next-day-same-meal '(:value event-576)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-607))

($make-named 'meal-event 'event-573 :name 'event-573 :patient-id 6
   :when '(:value 2750605200 :date Mar-01-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 107)
   :dose '(:value 4) :mod '(:value 0)
   :prev-meal-event '(:value event-572) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-574)
   :prev-day-same-meal '(:value event-569) :next-day-same-meal '(:value event-577)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-608))

($make-named 'meal-event 'event-574 :name 'event-574 :patient-id 6
   :when '(:value 2750626800 :date Mar-01-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 99)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-573) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-575)
   :prev-day-same-meal '(:value event-570) :next-day-same-meal '(:value event-578)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-609))

($make-named 'meal-event 'event-575 :name 'event-575 :patient-id 6
   :when '(:value 2750643000 :date Mar-01-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 63)
   :dose '(:value 0) :mod '(:value -2)
   :comment '(:value (cheetos cheese curls)
              :parsed t
              :calorie-type increased
              :calorie-degree moderate)
   :prev-meal-event '(:value event-574) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-576)
   :prev-day-same-meal '(:value event-571) :next-day-same-meal '(:value event-579)
   :prev-week-same-meal '(:value nil) :next-week-same-meal '(:value event-610))

($make-named 'meal-event 'event-576 :name 'event-576 :patient-id 6
   :when '(:value 2750677200 :date Mar-02-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 215)
   :urine '(:sugar 3% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 8) :mod '(:value 2)
   :prev-meal-event '(:value event-575) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-577)
   :prev-day-same-meal '(:value event-572) :next-day-same-meal '(:value event-580)
   :prev-week-same-meal '(:value event-545) :next-week-same-meal '(:value event-611))

($make-named 'meal-event 'event-577 :name 'event-577 :patient-id 6
   :when '(:value 2750689800 :date Mar-02-87 :time 1130)
   :meal '(:value lunch)
   :bg '(:value 60)
   :dose '(:value 5) :mod '(:value 1)
   :comment '(:value (2 for pizza)
              :parsed t
              :explain-dose-change calorie
              :dose-type reg
              :dose-adjustment 2
              :calorie-type increased
              :calorie-degree severe)
   :prev-meal-event '(:value event-576) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-578)
   :prev-day-same-meal '(:value event-573) :next-day-same-meal '(:value event-581)
   :prev-week-same-meal '(:value event-546) :next-week-same-meal '(:value event-613))

($make-named 'meal-event 'event-578 :name 'event-578 :patient-id 6
   :when '(:value 2750713200 :date Mar-02-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 164)
   :dose '(:value 7) :mod '(:value 1)
   :prev-meal-event '(:value event-577) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-579)
   :prev-day-same-meal '(:value event-574) :next-day-same-meal '(:value event-583)
   :prev-week-same-meal '(:value event-547) :next-week-same-meal '(:value event-614))

($make-named 'meal-event 'event-579 :name 'event-579 :patient-id 6
   :when '(:value 2750729400 :date Mar-02-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 121)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-578) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-580)
   :prev-day-same-meal '(:value event-575) :next-day-same-meal '(:value event-584)
   :prev-week-same-meal '(:value event-548) :next-week-same-meal '(:value event-615))

($make-named 'meal-event 'event-580 :name 'event-580 :patient-id 6
   :when '(:value 2750763600 :date Mar-03-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 122)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (slept on couch)
              :parsed t
              :activity-type decreased
              :activity-degree moderate)
   :prev-meal-event '(:value event-579) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-581)
   :prev-day-same-meal '(:value event-576) :next-day-same-meal '(:value event-585)
   :prev-week-same-meal '(:value event-549) :next-week-same-meal '(:value event-616))

($make-named 'meal-event 'event-581 :name 'event-581 :patient-id 6
   :when '(:value 2750778000 :date Mar-03-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 161)
   :dose '(:value 5) :mod '(:value 1)
   :comment '(:value (doing errands)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-580) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-582)) :next-meal-event '(:value event-583)
   :prev-day-same-meal '(:value event-577) :next-day-same-meal '(:value event-586)
   :prev-week-same-meal '(:value event-550) :next-week-same-meal '(:value event-617))

($make-named 'non-meal-event 'event-582 :name 'event-582 :patient-id 6
   :when '(:value 2750793600 :date Mar-03-87 :time 1620)
   :meal '(:value afternoon)
   :bg '(:value 55)
   :prev-meal-event '(:value event-581) :next-meal-event '(:value event-583))

($make-named 'meal-event 'event-583 :name 'event-583 :patient-id 6
   :when '(:value 2750799600 :date Mar-03-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 133)
   :dose '(:value 7) :mod '(:value 1)
   :prev-meal-event '(:value event-581) :pre-meal-events '(:value (event-582))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-584)
   :prev-day-same-meal '(:value event-578) :next-day-same-meal '(:value event-588)
   :prev-week-same-meal '(:value event-551) :next-week-same-meal '(:value event-618))

($make-named 'meal-event 'event-584 :name 'event-584 :patient-id 6
   :when '(:value 2750815800 :date Mar-03-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 118)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-583) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-585)
   :prev-day-same-meal '(:value event-579) :next-day-same-meal '(:value event-589)
   :prev-week-same-meal '(:value event-553) :next-week-same-meal '(:value event-619))

($make-named 'meal-event 'event-585 :name 'event-585 :patient-id 6
   :when '(:value 2750850000 :date Mar-04-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 149)
   :urine '(:sugar 1/4% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 7) :mod '(:value 1)
   :comment '(:value (walking #\, housework)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-584) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-586)
   :prev-day-same-meal '(:value event-580) :next-day-same-meal '(:value event-590)
   :prev-week-same-meal '(:value event-555) :next-week-same-meal '(:value event-620))

($make-named 'meal-event 'event-586 :name 'event-586 :patient-id 6
   :when '(:value 2750864400 :date Mar-04-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 62)
   :dose '(:value 3) :mod '(:value -1)
   :prev-meal-event '(:value event-585) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-587)) :next-meal-event '(:value event-588)
   :prev-day-same-meal '(:value event-581) :next-day-same-meal '(:value event-591)
   :prev-week-same-meal '(:value event-556) :next-week-same-meal '(:value event-621))

($make-named 'non-meal-event 'event-587 :name 'event-587 :patient-id 6
   :when '(:value 2750881200 :date Mar-04-87 :time 1640)
   :meal '(:value afternoon)
   :bg '(:value 45)
   :comment '(:value (took 2 tabs & 1 starch)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction over)
   :prev-meal-event '(:value event-586) :next-meal-event '(:value event-588))

($make-named 'meal-event 'event-588 :name 'event-588 :patient-id 6
   :when '(:value 2750886000 :date Mar-04-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 163)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-586) :pre-meal-events '(:value (event-587))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-589)
   :prev-day-same-meal '(:value event-583) :next-day-same-meal '(:value event-593)
   :prev-week-same-meal '(:value event-557) :next-week-same-meal '(:value event-623))

($make-named 'meal-event 'event-589 :name 'event-589 :patient-id 6
   :when '(:value 2750902200 :date Mar-04-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 142)
   :dose '(:value 3) :mod '(:value 1)
   :comment '(:value (ate too much)
              :parsed t
              :calorie-type increased
              :calorie-degree moderate)
   :prev-meal-event '(:value event-588) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-590)
   :prev-day-same-meal '(:value event-584) :next-day-same-meal '(:value event-594)
   :prev-week-same-meal '(:value event-558) :next-week-same-meal '(:value event-624))

($make-named 'meal-event 'event-590 :name 'event-590 :patient-id 6
   :when '(:value 2750936400 :date Mar-05-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 135)
   :urine '(:sugar 5% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 7) :mod '(:value 1)
   :comment '(:value (slept on couch)
              :parsed t
              :activity-type decreased
              :activity-degree severe)
   :prev-meal-event '(:value event-589) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-591)
   :prev-day-same-meal '(:value event-585) :next-day-same-meal '(:value event-596)
   :prev-week-same-meal '(:value event-559) :next-week-same-meal '(:value event-626))

($make-named 'meal-event 'event-591 :name 'event-591 :patient-id 6
   :when '(:value 2750950800 :date Mar-05-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 62)
   :dose '(:value 3) :mod '(:value -1)
   :comment '(:value (vacuuming house)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-590) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-592)) :next-meal-event '(:value event-593)
   :prev-day-same-meal '(:value event-586) :next-day-same-meal '(:value event-597)
   :prev-week-same-meal '(:value event-560) :next-week-same-meal '(:value event-627))

($make-named 'non-meal-event 'event-592 :name 'event-592 :patient-id 6
   :when '(:value 2750966100 :date Mar-05-87 :time 1615)
   :meal '(:value afternoon)
   :bg '(:value 56)
   :comment '(:value (took 2 tabs & 2 starch)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction over)
   :prev-meal-event '(:value event-591) :next-meal-event '(:value event-593))

($make-named 'meal-event 'event-593 :name 'event-593 :patient-id 6
   :when '(:value 2750976000 :date Mar-05-87 :time 1900)
   :meal '(:value dinner)
   :bg '(:value 161)
   :dose '(:value 7) :mod '(:value 1)
   :prev-meal-event '(:value event-591) :pre-meal-events '(:value (event-592))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-594)
   :prev-day-same-meal '(:value event-588) :next-day-same-meal '(:value event-599)
   :prev-week-same-meal '(:value event-561) :next-week-same-meal '(:value event-628))

($make-named 'meal-event 'event-594 :name 'event-594 :patient-id 6
   :when '(:value 2750988600 :date Mar-05-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 140)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-593) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-595)) :next-meal-event '(:value event-596)
   :prev-day-same-meal '(:value event-589) :next-day-same-meal '(:value event-600)
   :prev-week-same-meal '(:value event-562) :next-week-same-meal '(:value event-629))

($make-named 'non-meal-event 'event-595 :name 'event-595 :patient-id 6
   :when '(:value 2751008400 :date Mar-05-87 :time 2800)
   :meal '(:value post-snack)
   :bg '(:value 80)
   :prev-meal-event '(:value event-594) :next-meal-event '(:value event-596))

($make-named 'meal-event 'event-596 :name 'event-596 :patient-id 6
   :when '(:value 2751022800 :date Mar-06-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 101)
   :urine '(:sugar tr :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-594) :pre-meal-events '(:value (event-595))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-597)
   :prev-day-same-meal '(:value event-590) :next-day-same-meal '(:value event-602)
   :prev-week-same-meal '(:value event-563) :next-week-same-meal '(:value event-632))

($make-named 'meal-event 'event-597 :name 'event-597 :patient-id 6
   :when '(:value 2751037200 :date Mar-06-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 242)
   :dose '(:value 5) :mod '(:value 2)
   :prev-meal-event '(:value event-596) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-598)) :next-meal-event '(:value event-599)
   :prev-day-same-meal '(:value event-591) :next-day-same-meal '(:value event-603)
   :prev-week-same-meal '(:value event-564) :next-week-same-meal '(:value event-633))

($make-named 'non-meal-event 'event-598 :name 'event-598 :patient-id 6
   :when '(:value 2751054300 :date Mar-06-87 :time 1645)
   :meal '(:value afternoon)
   :bg '(:value 60)
   :comment '(:value (took 1 tab)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction correct)
   :prev-meal-event '(:value event-597) :next-meal-event '(:value event-599))

($make-named 'meal-event 'event-599 :name 'event-599 :patient-id 6
   :when '(:value 2751058800 :date Mar-06-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 92)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-597) :pre-meal-events '(:value (event-598))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-600)
   :prev-day-same-meal '(:value event-593) :next-day-same-meal '(:value event-604)
   :prev-week-same-meal '(:value event-565) :next-week-same-meal '(:value event-634))

($make-named 'meal-event 'event-600 :name 'event-600 :patient-id 6
   :when '(:value 2751073200 :date Mar-06-87 :time 2200)
   :meal '(:value snack)
   :bg '(:value 76)
   :dose '(:value 1) :mod '(:value -1)
   :prev-meal-event '(:value event-599) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-602)
   :prev-day-same-meal '(:value event-594) :next-day-same-meal '(:value event-605)
   :prev-week-same-meal '(:value event-566) :next-week-same-meal '(:value event-635))

($make-named 'comment-event 'event-601 :name 'event-601 :patient-id 6
   :when '(:value 2750994000 :date Mar-06-87 :time 0)
   :comment '(:value (will be more active #\; y decreased to 3)
              :parsed t))

($make-named 'meal-event 'event-602 :name 'event-602 :patient-id 6
   :when '(:value 2751109200 :date Mar-07-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 142)
   :urine '(:sugar 3% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 1)
   :prev-meal-event '(:value event-600) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-603)
   :prev-day-same-meal '(:value event-596) :next-day-same-meal '(:value event-607)
   :prev-week-same-meal '(:value event-568) :next-week-same-meal '(:value event-637))

($make-named 'meal-event 'event-603 :name 'event-603 :patient-id 6
   :when '(:value 2751123600 :date Mar-07-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 151)
   :dose '(:value 4) :mod '(:value 1)
   :prev-meal-event '(:value event-602) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-604)
   :prev-day-same-meal '(:value event-597) :next-day-same-meal '(:value event-608)
   :prev-week-same-meal '(:value event-569) :next-week-same-meal '(:value event-638))

($make-named 'meal-event 'event-604 :name 'event-604 :patient-id 6
   :when '(:value 2751145200 :date Mar-07-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 163)
   :dose '(:value 7) :mod '(:value 1)
   :comment '(:value (walking)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-603) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-605)
   :prev-day-same-meal '(:value event-599) :next-day-same-meal '(:value event-609)
   :prev-week-same-meal '(:value event-570) :next-week-same-meal '(:value event-639))

($make-named 'meal-event 'event-605 :name 'event-605 :patient-id 6
   :when '(:value 2751161400 :date Mar-07-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 61)
   :dose '(:value 0) :mod '(:value -2)
   :prev-meal-event '(:value event-604) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-607)
   :prev-day-same-meal '(:value event-600) :next-day-same-meal '(:value event-610)
   :prev-week-same-meal '(:value event-571) :next-week-same-meal '(:value event-640))

($make-named 'comment-event 'event-606 :name 'event-606 :patient-id 6
   :when '(:value 2751080400 :date Mar-07-87 :time 0)
   :comment '(:value (warts frozen #\; x decreased to 5)
              :parsed t))

($make-named 'meal-event 'event-607 :name 'event-607 :patient-id 6
   :when '(:value 2751195600 :date Mar-08-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 170)
   :urine '(:sugar 3% :acetone Sm)
   :longact '(:value 1.1)
   :dose '(:value 6) :mod '(:value 1)
   :prev-meal-event '(:value event-605) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-608)
   :prev-day-same-meal '(:value event-602) :next-day-same-meal '(:value event-611)
   :prev-week-same-meal '(:value event-572) :next-week-same-meal '(:value event-642))

($make-named 'meal-event 'event-608 :name 'event-608 :patient-id 6
   :when '(:value 2751210000 :date Mar-08-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 119)
   :dose '(:value 3) :mod '(:value 0)
   :prev-meal-event '(:value event-607) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-609)
   :prev-day-same-meal '(:value event-603) :next-day-same-meal '(:value event-613)
   :prev-week-same-meal '(:value event-573) :next-week-same-meal '(:value event-644))

($make-named 'meal-event 'event-609 :name 'event-609 :patient-id 6
   :when '(:value 2751231600 :date Mar-08-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 99)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-608) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-610)
   :prev-day-same-meal '(:value event-604) :next-day-same-meal '(:value event-614)
   :prev-week-same-meal '(:value event-574) :next-week-same-meal '(:value event-645))

($make-named 'meal-event 'event-610 :name 'event-610 :patient-id 6
   :when '(:value 2751249600 :date Mar-08-87 :time 2300)
   :meal '(:value snack)
   :bg '(:value 64)
   :dose '(:value 0) :mod '(:value -2)
   :comment '(:value (unexplained)
              :parsed t)
;              :reaction-degree mild
;              :treated-reaction unknown)
   :prev-meal-event '(:value event-609) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-611)
   :prev-day-same-meal '(:value event-605) :next-day-same-meal '(:value event-615)
   :prev-week-same-meal '(:value event-575) :next-week-same-meal '(:value event-646))

($make-named 'meal-event 'event-611 :name 'event-611 :patient-id 6
   :when '(:value 2751283800 :date Mar-09-87 :time 830)
   :meal '(:value breakfast)
   :bg '(:value 296)
   :urine '(:sugar 5% :acetone Lg)
   :longact '(:value 1.1)
   :dose '(:value 10) :mod '(:value 5)
   :comment '(:value (2)
              :parsed t
              :explain-dose-change ketonuria
              :dose-type reg
              :dose-adjustment 2)
   :prev-meal-event '(:value event-610) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-612)) :next-meal-event '(:value event-613)
   :prev-day-same-meal '(:value event-607) :next-day-same-meal '(:value event-616)
   :prev-week-same-meal '(:value event-576) :next-week-same-meal '(:value event-649))

($make-named 'non-meal-event 'event-612 :name 'event-612 :patient-id 6
   :when '(:value 2751291000 :date Mar-09-87 :time 1030)
   :meal '(:value morning)
   :bg '(:value 296)
   :urine '(:sugar 5% :acetone Med)
   :prev-meal-event '(:value event-611) :next-meal-event '(:value event-613))

($make-named 'meal-event 'event-613 :name 'event-613 :patient-id 6
   :when '(:value 2751296400 :date Mar-09-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 194)
   :urine '(:sugar 2% :acetone Neg)
   :dose '(:value 5) :mod '(:value 2)
   :comment '(:value (1)
              :parsed t
              :explain-dose-change ketonuria
              :dose-type reg
              :dose-adjustment 1)
   :prev-meal-event '(:value event-611) :pre-meal-events '(:value (event-612))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-614)
   :prev-day-same-meal '(:value event-608) :next-day-same-meal '(:value event-617)
   :prev-week-same-meal '(:value event-577) :next-week-same-meal '(:value event-650))

($make-named 'meal-event 'event-614 :name 'event-614 :patient-id 6
   :when '(:value 2751318000 :date Mar-09-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 81)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-613) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-615)
   :prev-day-same-meal '(:value event-609) :next-day-same-meal '(:value event-618)
   :prev-week-same-meal '(:value event-578) :next-week-same-meal '(:value event-651))

($make-named 'meal-event 'event-615 :name 'event-615 :patient-id 6
   :when '(:value 2751334200 :date Mar-09-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 123)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-614) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-616)
   :prev-day-same-meal '(:value event-610) :next-day-same-meal '(:value event-619)
   :prev-week-same-meal '(:value event-579) :next-week-same-meal '(:value event-652))

($make-named 'meal-event 'event-616 :name 'event-616 :patient-id 6
   :when '(:value 2751368400 :date Mar-10-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 74)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 4) :mod '(:value -1)
   :comment '(:value (-1 for activity)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -1
              :activity-type increased
              :activity-degree mild)
   :prev-meal-event '(:value event-615) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-617)
   :prev-day-same-meal '(:value event-611) :next-day-same-meal '(:value event-620)
   :prev-week-same-meal '(:value event-580) :next-week-same-meal '(:value event-653))

($make-named 'meal-event 'event-617 :name 'event-617 :patient-id 6
   :when '(:value 2751382800 :date Mar-10-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 145)
   :dose '(:value 4) :mod '(:value 1)
   :prev-meal-event '(:value event-616) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-618)
   :prev-day-same-meal '(:value event-613) :next-day-same-meal '(:value event-621)
   :prev-week-same-meal '(:value event-581) :next-week-same-meal '(:value event-655))

($make-named 'meal-event 'event-618 :name 'event-618 :patient-id 6
   :when '(:value 2751400800 :date Mar-10-87 :time 1700)
   :meal '(:value dinner)
   :bg '(:value 120)
   :dose '(:value 2) :mod '(:value 0)
   :comment '(:value (switch dinner/snack)
              :parsed t
              :explain-dose-change (meal-switch snack)
              :dose-type reg)
   :prev-meal-event '(:value event-617) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-619)
   :prev-day-same-meal '(:value event-614) :next-day-same-meal '(:value event-623)
   :prev-week-same-meal '(:value event-583) :next-week-same-meal '(:value event-656))

($make-named 'meal-event 'event-619 :name 'event-619 :patient-id 6
   :when '(:value 2751420600 :date Mar-10-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 172)
   :dose '(:value 7) :mod '(:value 1)
   :comment '(:value (dinner meal)
              :parsed t
              :explain-dose-change (meal-switch dinner)
              :dose-type reg)
   :prev-meal-event '(:value event-618) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-620)
   :prev-day-same-meal '(:value event-615) :next-day-same-meal '(:value event-624)
   :prev-week-same-meal '(:value event-584) :next-week-same-meal '(:value event-658))

($make-named 'meal-event 'event-620 :name 'event-620 :patient-id 6
   :when '(:value 2751454800 :date Mar-11-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 135)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 5) :mod '(:value 0)
   :comment '(:value (-1 for activity #\; sinus headache)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -1
              :activity-type increased
              :activity-degree mild
              :physiology-type illness
              :physiology-degree mild)
   :prev-meal-event '(:value event-619) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-621)
   :prev-day-same-meal '(:value event-616) :next-day-same-meal '(:value event-626)
   :prev-week-same-meal '(:value event-585) :next-week-same-meal '(:value event-661))

($make-named 'meal-event 'event-621 :name 'event-621 :patient-id 6
   :when '(:value 2751469200 :date Mar-11-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 230)
   :dose '(:value 5) :mod '(:value 2)
   :comment '(:value (walking)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-620) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-622)) :next-meal-event '(:value event-623)
   :prev-day-same-meal '(:value event-617) :next-day-same-meal '(:value event-627)
   :prev-week-same-meal '(:value event-586) :next-week-same-meal '(:value event-663))

($make-named 'non-meal-event 'event-622 :name 'event-622 :patient-id 6
   :when '(:value 2751487200 :date Mar-11-87 :time 1700)
   :meal '(:value afternoon)
   :bg '(:value 54)
   :comment '(:value (took 1 tab & 1 starch)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction correct)
   :prev-meal-event '(:value event-621) :next-meal-event '(:value event-623))

($make-named 'meal-event 'event-623 :name 'event-623 :patient-id 6
   :when '(:value 2751490800 :date Mar-11-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 127)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-621) :pre-meal-events '(:value (event-622))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-624)
   :prev-day-same-meal '(:value event-618) :next-day-same-meal '(:value event-628)
   :prev-week-same-meal '(:value event-588) :next-week-same-meal '(:value event-665))

($make-named 'meal-event 'event-624 :name 'event-624 :patient-id 6
   :when '(:value 2751507000 :date Mar-11-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 192)
   :dose '(:value 2) :mod '(:value 0)
   :comment '(:value (presumed rebound)
              :parsed t
              :explain-dose-change rebound
              :dose-type reg
              :dose-adjustment -1)
   :prev-meal-event '(:value event-623) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-626)
   :prev-day-same-meal '(:value event-619) :next-day-same-meal '(:value event-629)
   :prev-week-same-meal '(:value event-589) :next-week-same-meal '(:value event-667))

($make-named 'comment-event 'event-625 :name 'event-625 :patient-id 6
   :when '(:value 2751426000 :date Mar-11-87 :time 0)
   :comment '(:value (basal decreased to 0.1 for activity)
              :parsed t))

($make-named 'meal-event 'event-626 :name 'event-626 :patient-id 6
   :when '(:value 2751541200 :date Mar-12-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 144)
   :urine '(:sugar 1% :acetone Neg)
   :longact '(:value 1.1)
   :dose '(:value 5) :mod '(:value -1)
   :comment '(:value (horseback riding)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-624) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-627)
   :prev-day-same-meal '(:value event-620) :next-day-same-meal '(:value event-632)
   :prev-week-same-meal '(:value event-590) :next-week-same-meal '(:value event-669))

($make-named 'meal-event 'event-627 :name 'event-627 :patient-id 6
   :when '(:value 2751555600 :date Mar-12-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 51)
   :dose '(:value 2) :mod '(:value -1)
   :comment '(:value (symptomatic)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction unknown)
   :prev-meal-event '(:value event-626) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-628)
   :prev-day-same-meal '(:value event-621) :next-day-same-meal '(:value event-633)
   :prev-week-same-meal '(:value event-591) :next-week-same-meal '(:value event-670))

($make-named 'meal-event 'event-628 :name 'event-628 :patient-id 6
   :when '(:value 2751577200 :date Mar-12-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 172)
   :dose '(:value 7) :mod '(:value 1)
   :prev-meal-event '(:value event-627) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-629)
   :prev-day-same-meal '(:value event-623) :next-day-same-meal '(:value event-634)
   :prev-week-same-meal '(:value event-593) :next-week-same-meal '(:value event-671))

($make-named 'meal-event 'event-629 :name 'event-629 :patient-id 6
   :when '(:value 2751593400 :date Mar-12-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 135)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-628) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-630)) :next-meal-event '(:value event-632)
   :prev-day-same-meal '(:value event-624) :next-day-same-meal '(:value event-635)
   :prev-week-same-meal '(:value event-594) :next-week-same-meal '(:value event-672))

($make-named 'non-meal-event 'event-630 :name 'event-630 :patient-id 6
   :when '(:value 2751611400 :date Mar-12-87 :time 2730)
   :meal '(:value post-snack)
   :bg '(:value 142)
   :prev-meal-event '(:value event-629) :next-meal-event '(:value event-632))

($make-named 'comment-event 'event-631 :name 'event-631 :patient-id 6
   :when '(:value 2751512400 :date Mar-12-87 :time 0)
   :comment '(:value (menses)
              :parsed t
              :physiology-type menses))

($make-named 'meal-event 'event-632 :name 'event-632 :patient-id 6
   :when '(:value 2751627600 :date Mar-13-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 128)
   :urine '(:sugar 1/4% :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 5) :mod '(:value 0)
   :prev-meal-event '(:value event-629) :pre-meal-events '(:value (event-630))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-633)
   :prev-day-same-meal '(:value event-626) :next-day-same-meal '(:value event-637)
   :prev-week-same-meal '(:value event-596) :next-week-same-meal '(:value event-674))

($make-named 'meal-event 'event-633 :name 'event-633 :patient-id 6
   :when '(:value 2751642000 :date Mar-13-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 230)
   :dose '(:value 5) :mod '(:value 2)
   :comment '(:value (walking)
              :parsed t
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-632) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-634)
   :prev-day-same-meal '(:value event-627) :next-day-same-meal '(:value event-638)
   :prev-week-same-meal '(:value event-597) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-634 :name 'event-634 :patient-id 6
   :when '(:value 2751663600 :date Mar-13-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 61)
   :dose '(:value 5) :mod '(:value -1)
   :prev-meal-event '(:value event-633) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-635)
   :prev-day-same-meal '(:value event-628) :next-day-same-meal '(:value event-639)
   :prev-week-same-meal '(:value event-599) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-635 :name 'event-635 :patient-id 6
   :when '(:value 2751679800 :date Mar-13-87 :time 2230)
   :meal '(:value snack)
   :dose '(:value 0) :mod '(:value -2)
   :comment '(:value (fell asleep 800 pm #\; no snack or insulin)
              :parsed t
              :explain-missing-bg asleep
              :explain-dose-change asleep
              :dose-type reg
              :dose-adjustment -2)
   :prev-meal-event '(:value event-634) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-637)
   :prev-day-same-meal '(:value event-629) :next-day-same-meal '(:value event-640)
   :prev-week-same-meal '(:value event-600) :next-week-same-meal '(:value nil))

($make-named 'comment-event 'event-636 :name 'event-636 :patient-id 6
   :when '(:value 2751598800 :date Mar-13-87 :time 0)
   :comment '(:value (basal increased to 1.2)
              :parsed t))

($make-named 'meal-event 'event-637 :name 'event-637 :patient-id 6
   :when '(:value 2751710400 :date Mar-14-87 :time 700)
   :meal '(:value breakfast)
   :bg '(:value 57)
   :urine '(:sugar tr :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 4) :mod '(:value -1)
   :prev-meal-event '(:value event-635) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-638)
   :prev-day-same-meal '(:value event-632) :next-day-same-meal '(:value event-642)
   :prev-week-same-meal '(:value event-602) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-638 :name 'event-638 :patient-id 6
   :when '(:value 2751728400 :date Mar-14-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 90)
   :dose '(:value 3) :mod '(:value 0)
   :prev-meal-event '(:value event-637) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-639)
   :prev-day-same-meal '(:value event-633) :next-day-same-meal '(:value event-644)
   :prev-week-same-meal '(:value event-603) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-639 :name 'event-639 :patient-id 6
   :when '(:value 2751750000 :date Mar-14-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 122)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (napping)
              :parsed t
              :activity-type decreased
              :activity-degree moderate)
   :prev-meal-event '(:value event-638) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-640)
   :prev-day-same-meal '(:value event-634) :next-day-same-meal '(:value event-645)
   :prev-week-same-meal '(:value event-604) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-640 :name 'event-640 :patient-id 6
   :when '(:value 2751771600 :date Mar-14-87 :time 2400)
   :meal '(:value snack)
   :bg '(:value 82)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-639) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-641)) :next-meal-event '(:value event-642)
   :prev-day-same-meal '(:value event-635) :next-day-same-meal '(:value event-646)
   :prev-week-same-meal '(:value event-605) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-641 :name 'event-641 :patient-id 6
   :when '(:value 2751786000 :date Mar-14-87 :time 2800)
   :meal '(:value post-snack)
   :bg '(:value 153)
   :prev-meal-event '(:value event-640) :next-meal-event '(:value event-642))

($make-named 'meal-event 'event-642 :name 'event-642 :patient-id 6
   :when '(:value 2751800400 :date Mar-15-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 123)
   :urine '(:sugar tr :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 5) :mod '(:value 0)
   :prev-meal-event '(:value event-640) :pre-meal-events '(:value (event-641))
   :post-meal-events '(:value (event-643)) :next-meal-event '(:value event-644)
   :prev-day-same-meal '(:value event-637) :next-day-same-meal '(:value event-649)
   :prev-week-same-meal '(:value event-607) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-643 :name 'event-643 :patient-id 6
   :when '(:value 2751807600 :date Mar-15-87 :time 1000)
   :meal '(:value morning)
   :comment '(:value (napping)
              :parsed t
              :duration 330
              :activity-type decreased
              :activity-degree moderate)
   :prev-meal-event '(:value event-642) :next-meal-event '(:value event-644))

($make-named 'meal-event 'event-644 :name 'event-644 :patient-id 6
   :when '(:value 2751820200 :date Mar-15-87 :time 1330)
   :meal '(:value lunch)
   :bg '(:value 75)
   :dose '(:value 3) :mod '(:value 0)
   :prev-meal-event '(:value event-642) :pre-meal-events '(:value (event-643))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-645)
   :prev-day-same-meal '(:value event-638) :next-day-same-meal '(:value event-650)
   :prev-week-same-meal '(:value event-608) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-645 :name 'event-645 :patient-id 6
   :when '(:value 2751838200 :date Mar-15-87 :time 1830)
   :meal '(:value dinner)
   :bg '(:value 118)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-644) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-646)
   :prev-day-same-meal '(:value event-639) :next-day-same-meal '(:value event-651)
   :prev-week-same-meal '(:value event-609) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-646 :name 'event-646 :patient-id 6
   :when '(:value 2751852600 :date Mar-15-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 138)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-645) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-647 event-648)) :next-meal-event '(:value event-649)
   :prev-day-same-meal '(:value event-640) :next-day-same-meal '(:value event-652)
   :prev-week-same-meal '(:value event-610) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-647 :name 'event-647 :patient-id 6
   :when '(:value 2751858000 :date Mar-15-87 :time 2400)
   :meal '(:value post-snack)
   :bg '(:value 125)
   :prev-meal-event '(:value event-646) :next-meal-event '(:value event-648))

($make-named 'non-meal-event 'event-648 :name 'event-648 :patient-id 6
   :when '(:value 2751872400 :date Mar-15-87 :time 2800)
   :meal '(:value post-snack)
   :bg '(:value 157)
   :prev-meal-event '(:value event-647) :next-meal-event '(:value event-649))

($make-named 'meal-event 'event-649 :name 'event-649 :patient-id 6
   :when '(:value 2751886800 :date Mar-16-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 112)
   :urine '(:sugar neg :acetone Tr)
   :longact '(:value 1.2)
   :dose '(:value 5) :mod '(:value 0)
   :prev-meal-event '(:value event-646) :pre-meal-events '(:value (event-648 event-647))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-650)
   :prev-day-same-meal '(:value event-642) :next-day-same-meal '(:value event-653)
   :prev-week-same-meal '(:value event-611) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-650 :name 'event-650 :patient-id 6
   :when '(:value 2751901200 :date Mar-16-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 218)
   :dose '(:value 5) :mod '(:value 2)
   :prev-meal-event '(:value event-649) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-651)
   :prev-day-same-meal '(:value event-644) :next-day-same-meal '(:value event-655)
   :prev-week-same-meal '(:value event-613) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-651 :name 'event-651 :patient-id 6
   :when '(:value 2751922800 :date Mar-16-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 117)
   :dose '(:value 6) :mod '(:value 0)
   :comment '(:value (frozen batter-dipped fish)
              :parsed t
              :calorie-type increased
              :calorie-degree severe) ;;;******** based on high-glycemic-index
   :prev-meal-event '(:value event-650) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-652)
   :prev-day-same-meal '(:value event-645) :next-day-same-meal '(:value event-656)
   :prev-week-same-meal '(:value event-614) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-652 :name 'event-652 :patient-id 6
   :when '(:value 2751939000 :date Mar-16-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 249)
   :dose '(:value 4) :mod '(:value 2)
   :prev-meal-event '(:value event-651) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-653)
   :prev-day-same-meal '(:value event-646) :next-day-same-meal '(:value event-658)
   :prev-week-same-meal '(:value event-615) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-653 :name 'event-653 :patient-id 6
   :when '(:value 2751973200 :date Mar-17-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 76)
   :urine '(:sugar neg :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 5) :mod '(:value 0)
   :prev-meal-event '(:value event-652) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-654)) :next-meal-event '(:value event-655)
   :prev-day-same-meal '(:value event-649) :next-day-same-meal '(:value event-661)
   :prev-week-same-meal '(:value event-616) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-654 :name 'event-654 :patient-id 6
   :when '(:value 2751978600 :date Mar-17-87 :time 930)
   :meal '(:value morning)
   :comment '(:value (1 for orange juice)
              :parsed t
              :explain-dose-change calorie
              :dose-type reg
              :dose-adjustment 1
              :calorie-type increased
              :calorie-degree severe)
   :prev-meal-event '(:value event-653) :next-meal-event '(:value event-655))

($make-named 'meal-event 'event-655 :name 'event-655 :patient-id 6
   :when '(:value 2751987600 :date Mar-17-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 180)
   :dose '(:value 3.5) :mod '(:value -0.5)
   :comment '(:value (-0.5 for work)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -0.5
              :activity-type increased
              :activity-degree mild)
   :prev-meal-event '(:value event-653) :pre-meal-events '(:value (event-654))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-656)
   :prev-day-same-meal '(:value event-650) :next-day-same-meal '(:value event-663)
   :prev-week-same-meal '(:value event-617) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-656 :name 'event-656 :patient-id 6
   :when '(:value 2752009200 :date Mar-17-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 89)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-655) :pre-meal-events '(:value nil)
   :post-meal-events '(:value (event-657)) :next-meal-event '(:value event-658)
   :prev-day-same-meal '(:value event-651) :next-day-same-meal '(:value event-665)
   :prev-week-same-meal '(:value event-618) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-657 :name 'event-657 :patient-id 6
   :when '(:value 2752016400 :date Mar-17-87 :time 2000)
   :meal '(:value evening)
   :bg '(:value 64)
   :comment '(:value (took 1 tab & 1 starch)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction correct)
   :prev-meal-event '(:value event-656) :next-meal-event '(:value event-658))

($make-named 'meal-event 'event-658 :name 'event-658 :patient-id 6
   :when '(:value 2752025400 :date Mar-17-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 112)
   :dose '(:value 2) :mod '(:value 0)
   :prev-meal-event '(:value event-656) :pre-meal-events '(:value (event-657))
   :post-meal-events '(:value (event-659)) :next-meal-event '(:value event-661)
   :prev-day-same-meal '(:value event-652) :next-day-same-meal '(:value event-667)
   :prev-week-same-meal '(:value event-619) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-659 :name 'event-659 :patient-id 6
   :when '(:value 2752041600 :date Mar-17-87 :time 2700)
   :meal '(:value post-snack)
   :bg '(:value 170)
   :prev-meal-event '(:value event-658) :next-meal-event '(:value event-661))

($make-named 'comment-event 'event-660 :name 'event-660 :patient-id 6
   :when '(:value 2752030800 :date Mar-18-87 :time 0)
   :comment '(:value (runny nose #\, sneezing #\, feels sick)
              :parsed t
              :physiology-type illness
              :physiology-degree moderate))

($make-named 'meal-event 'event-661 :name 'event-661 :patient-id 6
   :when '(:value 2752059600 :date Mar-18-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 145)
   :urine '(:sugar tr :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 6) :mod '(:value 1)
   :prev-meal-event '(:value event-658) :pre-meal-events '(:value (event-659))
   :post-meal-events '(:value (event-662)) :next-meal-event '(:value event-663)
   :prev-day-same-meal '(:value event-653) :next-day-same-meal '(:value event-669)
   :prev-week-same-meal '(:value event-620) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-662 :name 'event-662 :patient-id 6
   :when '(:value 2752066800 :date Mar-18-87 :time 1000)
   :meal '(:value morning)
   :bg '(:value 192)
   :comment '(:value (2 hr. post-prandial)
              :parsed t
              :explain-extra-bg 2hr-pp)
   :prev-meal-event '(:value event-661) :next-meal-event '(:value event-663))

($make-named 'meal-event 'event-663 :name 'event-663 :patient-id 6
   :when '(:value 2752074000 :date Mar-18-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 218)
   :dose '(:value 5) :mod '(:value 2)
   :prev-meal-event '(:value event-661) :pre-meal-events '(:value (event-662))
   :post-meal-events '(:value (event-664)) :next-meal-event '(:value event-665)
   :prev-day-same-meal '(:value event-655) :next-day-same-meal '(:value event-670)
   :prev-week-same-meal '(:value event-621) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-664 :name 'event-664 :patient-id 6
   :when '(:value 2752086600 :date Mar-18-87 :time 1530)
   :meal '(:value afternoon)
   :bg '(:value 215)
   :comment '(:value (2 hr. post-prandial)
              :parsed t
              :explain-extra-bg 2hr-pp)
   :prev-meal-event '(:value event-663) :next-meal-event '(:value event-665))

($make-named 'meal-event 'event-665 :name 'event-665 :patient-id 6
   :when '(:value 2752095600 :date Mar-18-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 202)
   :dose '(:value 8) :mod '(:value 2)
   :prev-meal-event '(:value event-663) :pre-meal-events '(:value (event-664))
   :post-meal-events '(:value (event-666)) :next-meal-event '(:value event-667)
   :prev-day-same-meal '(:value event-656) :next-day-same-meal '(:value event-671)
   :prev-week-same-meal '(:value event-623) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-666 :name 'event-666 :patient-id 6
   :when '(:value 2752104600 :date Mar-18-87 :time 2030)
   :meal '(:value evening)
   :bg '(:value 72)
   :comment '(:value (basal decreased to 0.6 for vacuuming #\; 2 hr. post-prandial)
              :parsed t
              :duration 45
              :explain-extra-bg 2hr-pp
              :explain-dose-change activity
              :dose-type CSI
              :dose-adjustment -0.6
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-665) :next-meal-event '(:value event-667))

($make-named 'meal-event 'event-667 :name 'event-667 :patient-id 6
   :when '(:value 2752106400 :date Mar-18-87 :time 2100)
   :meal '(:value snack)
   :bg '(:value 65)
   :dose '(:value 2) :mod '(:value 0)
   :comment '(:value (symptomatic)
              :parsed t
;              :reaction-degree moderate
              :treated-reaction unknown)
   :prev-meal-event '(:value event-665) :pre-meal-events '(:value (event-666))
   :post-meal-events '(:value (event-668)) :next-meal-event '(:value event-669)
   :prev-day-same-meal '(:value event-658) :next-day-same-meal '(:value event-672)
   :prev-week-same-meal '(:value event-624) :next-week-same-meal '(:value nil))

($make-named 'non-meal-event 'event-668 :name 'event-668 :patient-id 6
   :when '(:value 2752113600 :date Mar-18-87 :time 2300)
   :meal '(:value post-snack)
   :bg '(:value 84)
   :comment '(:value (2 hr. post-prandial)
              :parsed t
              :explain-extra-bg 2hr-pp)
   :prev-meal-event '(:value event-667) :next-meal-event '(:value event-669))

($make-named 'meal-event 'event-669 :name 'event-669 :patient-id 6
   :when '(:value 2752146000 :date Mar-19-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 118)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-667) :pre-meal-events '(:value (event-668))
   :post-meal-events '(:value nil) :next-meal-event '(:value event-670)
   :prev-day-same-meal '(:value event-661) :next-day-same-meal '(:value event-674)
   :prev-week-same-meal '(:value event-626) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-670 :name 'event-670 :patient-id 6
   :when '(:value 2752160400 :date Mar-19-87 :time 1200)
   :meal '(:value lunch)
   :bg '(:value 168)
   :dose '(:value 4) :mod '(:value 1)
   :prev-meal-event '(:value event-669) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-671)
   :prev-day-same-meal '(:value event-663) :next-day-same-meal '(:value nil)
   :prev-week-same-meal '(:value event-627) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-671 :name 'event-671 :patient-id 6
   :when '(:value 2752182000 :date Mar-19-87 :time 1800)
   :meal '(:value dinner)
   :bg '(:value 73)
   :dose '(:value 6) :mod '(:value 0)
   :prev-meal-event '(:value event-670) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-672)
   :prev-day-same-meal '(:value event-665) :next-day-same-meal '(:value nil)
   :prev-week-same-meal '(:value event-628) :next-week-same-meal '(:value nil))

($make-named 'meal-event 'event-672 :name 'event-672 :patient-id 6
   :when '(:value 2752198200 :date Mar-19-87 :time 2230)
   :meal '(:value snack)
   :bg '(:value 64)
   :dose '(:value 0) :mod '(:value -2)
   :prev-meal-event '(:value event-671) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value event-674)
   :prev-day-same-meal '(:value event-667) :next-day-same-meal '(:value nil)
   :prev-week-same-meal '(:value event-629) :next-week-same-meal '(:value nil))

($make-named 'comment-event 'event-673 :name 'event-673 :patient-id 6
   :when '(:value 2752117200 :date Mar-19-87 :time 0)
   :comment '(:value (x increased to 6)
              :parsed t))

($make-named 'meal-event 'event-674 :name 'event-674 :patient-id 6
   :when '(:value 2752232400 :date Mar-20-87 :time 800)
   :meal '(:value breakfast)
   :bg '(:value 192)
   :urine '(:sugar 1/2% :acetone Neg)
   :longact '(:value 1.2)
   :dose '(:value 5) :mod '(:value -1)
   :comment '(:value (-1 for walking)
              :parsed t
              :explain-dose-change activity
              :dose-type reg
              :dose-adjustment -1
              :activity-type increased
              :activity-degree moderate)
   :prev-meal-event '(:value event-672) :pre-meal-events '(:value nil)
   :post-meal-events '(:value nil) :next-meal-event '(:value nil)
   :prev-day-same-meal '(:value event-669) :next-day-same-meal '(:value nil)
   :prev-week-same-meal '(:value event-632) :next-week-same-meal '(:value nil))
