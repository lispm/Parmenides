
(def-frame action ()
            :actor   (:default 'agent)
            :object  (:default 'physical-object))

(def-frame ptrans (:is-a action)
            :from    (:default 'location)
            :to      (:default 'location))

(make-frame 'ptrans 'p1)
