;;; Tests the ability of Parmenides to handle (i.e., compile) files
;;; with nested frames in them.
(def-frame citizen () ()
  pays yes)

(make-citizen 'c1 :pays '(value no))

(def-frame criminal () ()
  ctype (value (frame 'c1)))
