
;; 
;;
;;
(defstruct (observation
	(:constructor make-observation (&key class dimensionality values)))
	(class 0 :type string)
	(dimensionality 0 :type integer)
	(distance 0.0 :type single-float)
	(values nil :type list))