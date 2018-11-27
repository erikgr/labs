;;
;;
;;


;;
;;
;;
(defun split-string (string delimiter)
    (loop for i = 0 then (1+ j)
          as j = (position delimiter string :start i)
          collect (subseq string i j)
          while j))

;;
;;
;;
(defun read-csv (&key file delimiter)
	(let ((csv (list)))
		(with-open-file (input file)
			(loop for line = (read-line input nil)
			while line do
			(if (eq csv nil)
				(setf csv (list (split-string line delimiter)))
				(push (split-string line delimiter) (cdr (last csv))))))
	(return-from read-csv csv)))