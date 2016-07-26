(def-frame person (:extendable nil)
  age (value depth)
  height (value depth))

(def-frame boy (:extendable t)
  size (value depth)
  money (value depth))
