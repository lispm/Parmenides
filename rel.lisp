(in-package 'pa)


(def-frame foo ())

(literalize bar1 ())

(eval-when (load eval)
  (def-frame* 'relation ()
    '(:combination-type :FIRST
      :slots-inherited (value :*ALL*)
      :has-inverses NIL	;; Indicates if the relation has inverses.
      :inverse-name NIL))
)
