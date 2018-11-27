;;
;;
;;

(load "KNN_structs.lisp")
(load "csv.lisp")
(load "hash_funcs.lisp")


(defparameter *model* nil)


(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))


;; stolen from https://groups.google.com/forum/#!topic/comp.lang.lisp/-W1FeuHq0DI
;;
;;
(defun parse-float (string)
  "Return a float read from string, and the index to the remainder of string."
  (multiple-value-bind (integer i)
      (parse-integer string :junk-allowed t)
    (multiple-value-bind (fraction j)
        (parse-integer string :start (+ i 1) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1)))))
      	j))))

;;
;;
;;
(defun string-to-list (s)
  (assert (stringp s) (s) "~s :questa non e una stringa")
  (coerce s 'list))

;; Jaccard distance (for string values)
;;
(defun distance-jaccard (a b)
	(- 1 (/	(list-length (intersection (string-to-list a) (string-to-list b)))
		(list-length (union (string-to-list a) (string-to-list b))))))

;; Pick a function to determine
;; distance between two values.
;;
(defun distance-for (var)
	(cond
		((stringp var) (return-from distance-for #'distance-jaccard))
		((integerp var) (return-from distance-for #'-))
		((floatp var) (return-from distance-for #'-))))

;; Calculate euclidean distance between
;; observations A and B.
;;
(defun distance (a b)
	(sqrt (reduce #'+ (mapcar (lambda (x) (* x x))
				(mapcar #'funcall (mapcar #'distance-for
					(observation-values a)) (observation-values a) (observation-values b))))))

;; Load observations from CSV.
;; Assign each observation a class from a field
;; at index 'class-idx'.
;;
(defun csv-to-observations (&key file delimiter class-idx)
	(let ((observations nil))
		(loop for line in (read-csv :file file :delimiter delimiter) do
			(let ((values (remove-nth class-idx line)) (class (nth class-idx line)))
				(if (eq observations nil)
				(setf observations (list (make-observation :class class :dimensionality (list-length values) :values (mapcar #'parse-integer values))))
				(push (make-observation :class class :dimensionality (list-length values) :values (mapcar #'parse-integer values)) (cdr (last observations))))))
		(return-from csv-to-observations observations)))

;; True if the calculated distance for
;; observation A is smaller than the calculated
;; distance for observation B
;;
(defun obs-dist-smallerp (a b)
	(< (observation-distance a) (observation-distance b)))

;; Calculate the distance to each
;; observation in the model
;;
(defun calculate-distances (observation)
	(loop for element in *model* do
		(setf (observation-distance element) (distance observation element))))

;; Return the k nearest neighbours for
;; observation 
;;
(defun KNN (observation k)
	(if (eq *model* nil)
		(format *standard-output* "ERROR: Can not perform KNN: model not built.")
		(progn
			(calculate-distances observation)
			(subseq (sort *model* #'obs-dist-smallerp) 0 k))))

;;
;;
;;
(defun classify (observation k)
	(let ((hash (make-hash-table)))
		(loop for obs in (knn observation k) do
			(hash-increment (observation-class obs) hash))
		(setf (observation-class observation) (caar (hashtable-top-n-values hash 1)))))


(defparameter *model* (csv-to-observations :file "haberman.csv" :delimiter #\; :class-idx 3))
(defparameter target (make-observation :class "?" :dimensionality 3 :values '(63 59 2)))