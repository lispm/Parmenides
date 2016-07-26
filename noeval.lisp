;;;;  No-eval patch for Parmenides
;;;;  Load this AFTER loading Parmenides if you want def-frame to not eval
;;;;  the contents of facts in instance slots.

(in-package "PARMENIDES")


(defun eval-plist (plist)
  plist)

(defun maybe-update-range-classes (name full-iplist full-cplist)
  (do ((plist full-iplist (cddr plist)))
      ((null plist))
    (let* ((usname (assure-current (car plist)))
	   (contents (cadr plist))
	   (facets (consp contents))
	   (value (if facets (cadr contents) contents)))
      (when (and value (invertible-relation usname))
	(let ((invsname (get-slot usname :inverse-name)))
	  (if (consp value)	;;multiple frames in range
	      (dolist (range value)
		(update-range-class-value invsname range name facets))
	      (update-range-class-value invsname value name facets))))))
  (do ((plist full-cplist (cddr plist)))
      ((null plist))
    (let* ((usname (assure-current (car plist)))
	   (value (cadr plist)))
      (when (and value (invertible-relation usname))
	(let ((invsname (get-slot usname :inverse-name)))
	  (if (consp value)	;;multiple frames in range
	      (dolist (range value)
		(update-range-class-cplist-value invsname range name))
	      (update-range-class-cplist-value invsname value name)))))))
