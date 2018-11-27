;;
;;
;;
;;

(defun hash-print (hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(loop for key being the hash-keys of hashtable
			do (format t "~s -> ~d~%" key (hash-get key hashtable)))))

(defun hash-put (key value hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(setf (gethash key hashtable) value)))

(defun hash-get (key hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(if (gethash key hashtable)
			(gethash key hashtable)
			(+ 0))))

(defun hash-increment (key hashtable)
	(hash-put key (+ 1 (hash-get key hashtable)) hashtable))

(defun hashtable-top-n-values (hashtable n)
  (subseq (sort (hashtable-alist hashtable) #'> :key #'cdr) 0 n))

(defun hashtable-alist (hashtable) 
	(let ((alist nil))
		(maphash (lambda (k v) (push (cons k v) alist))	hashtable) alist))