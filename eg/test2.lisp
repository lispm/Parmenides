;; -*- Mode: Lisp; Package: USER -*-

(defun movem-all ()
  (dolist (event (get-instances 'non-meal-event))
    ($move-frame event)))

(defun $move-frame (event)
  (let ((save-old-frame (frame event))
	(temp-name (gensym)))
    ($make-named 'reaction-wo-meal-event temp-name :related-to event)
    ($remove save-old-frame)
    (rename-frame temp-name event)
    (set-slot event :related-to event)))

